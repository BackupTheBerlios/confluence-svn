/*
    FNF: Free Netlist Format
    Copyright (C) 2004 Tom Hawkins (tomahawkins@yahoo.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
*/


#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "ivl_target.h"

// Output file.
FILE* output = 0;

// Design.
ivl_design_t design = 0;

// Current level of design.
unsigned level = 0;

// Write indent.
void indent()
{
  unsigned i;
  for (i = 0; i < level; i++)
    fprintf(output, "  ");
}

// Write quoted string.
void quoted_string(const char * string)
{
  int i = 0;
  fprintf(output, "\"");
  while (string[i] != '\0') {
    if (string[i] == '"')
      fprintf(output, "\\\"");
    else if (string[i] == '\\')
      fprintf(output, "\\\\");
    else
      fprintf(output, "%c", string[i]);
    i++;
  }
  fprintf(output, "\"");
}

// Nexus id table.
struct nexus_table_s {
  ivl_nexus_t nexus;
  unsigned id;
  struct nexus_table_s* lt;
  struct nexus_table_s* gt;
} * nexus_table = 0;
typedef struct nexus_table_s *nexus_table_t;
unsigned next_id = 0;

// Create next id.
unsigned new_id()
{
  return next_id++;
}

// Delete the nexus table.
void delete_nexus_table(nexus_table_t table)
{ 
  if (table) {
    nexus_table_t lt = table->lt;
    nexus_table_t gt = table->gt;
    free((void*) table);
    delete_nexus_table(lt);
    delete_nexus_table(gt);
  }
}

// Table construction.
unsigned id_of_nexus_0(ivl_nexus_t nexus, nexus_table_t table)
{
  if (nexus == table->nexus)
    return table->id;
  else if (nexus < table->nexus)
    if (table->lt)
      return id_of_nexus_0(nexus, table->lt);
    else {
      table->lt = (nexus_table_t) (malloc(sizeof(*table)));
      table->lt->nexus = nexus;
      table->lt->id = new_id();
      table->lt->lt = 0;
      table->lt->gt = 0;
      return table->lt->id;
    }
  else // nexus > table->nexus
    if (table->gt)
      return id_of_nexus_0(nexus, table->gt);
    else {
      table->gt = (nexus_table_t) (malloc(sizeof(*table)));
      table->gt->nexus = nexus;
      table->gt->id = new_id();
      table->gt->lt = 0;
      table->gt->gt = 0;
      return table->gt->id;
    }
}

// Lookup nexus id.
unsigned id_of_nexus(ivl_nexus_t nexus)
{
  if (! nexus_table) {
    nexus_table = (nexus_table_t) (malloc(sizeof(*nexus_table)));
    nexus_table->nexus = nexus;
    nexus_table->id = new_id();
    nexus_table->lt = 0;
    nexus_table->gt = 0;
    return nexus_table->id;
  }
  else
    return id_of_nexus_0(nexus, nexus_table);
}


// Create single bit select.
void create_bit_select(unsigned output_id, unsigned width, unsigned bit, unsigned input_id)
{
  indent();
  fprintf(output, "  (select %i %i %i %i)\n", output_id, width, bit, input_id);
}

// Create bit concat.
unsigned create_bit_concat(unsigned input_id, unsigned input_width, ivl_nexus_t nexus)
{
  unsigned id = new_id();
  indent();
  fprintf(output, "  (concat %i 1 %i %i %i)\n", id, input_width, id_of_nexus(nexus), input_id);
  return id;
}

// Concat lpm_data
unsigned create_concat_lpm_data(ivl_lpm_t lpm)
{
  unsigned width = ivl_lpm_width(lpm);
  unsigned id;
  unsigned i;
  id = id_of_nexus(ivl_lpm_data(lpm, 0));
  for (i = 1; i < width; i++)
    id = create_bit_concat(id, i, ivl_lpm_data(lpm, i));
  return id;
}
  
// Concat lpm_datab
unsigned create_concat_lpm_datab(ivl_lpm_t lpm)
{
  unsigned width = ivl_lpm_width(lpm);
  unsigned id;
  unsigned i;
  id = id_of_nexus(ivl_lpm_datab(lpm, 0));
  for (i = 1; i < width; i++)
    id = create_bit_concat(id, i, ivl_lpm_datab(lpm, i));
  return id;
}

// Concat lpm_data2
unsigned create_concat_lpm_data2(ivl_lpm_t lpm, unsigned select)
{
  unsigned width = ivl_lpm_width(lpm);
  unsigned id;
  unsigned i;
  id = id_of_nexus(ivl_lpm_data2(lpm, select, 0));
  for (i = 1; i < width; i++)
    id = create_bit_concat(id, i, ivl_lpm_data2(lpm, select, i));
  return id;
}

// Split lpm_q
void create_split_lpm_q(ivl_lpm_t lpm, unsigned id)
{
  unsigned width = ivl_lpm_width(lpm);
  unsigned i;
  for (i = 0; i < width; i++)
    create_bit_select(id_of_nexus(ivl_lpm_q(lpm, i)), width, i, id);
}
  
// Repeated gates.
void create_multi_gate(const char* gate, unsigned id, ivl_net_logic_t logic)
{
  unsigned width = ivl_logic_pins(logic);
  unsigned id1 = id_of_nexus(ivl_logic_pin(logic, 1));
  if (width == 2) {
    indent();
    fprintf(output, "  (buf    %i 1 %i)\n", id, id1);
  }
  else {
    unsigned i;
    unsigned id2;
    for (i = 2; i < width; i++) {
      id2 = (i == width - 1) ? id : new_id();
      indent();
      fprintf(output, "  (%s   %i 1 %i %i)\n", gate, id2, id1, id_of_nexus(ivl_logic_pin(logic, i)));
      id1 = id2;
    }
  }
}

// Mux tree.
unsigned create_mux(ivl_lpm_t lpm, unsigned select_width, unsigned select_number)
{
  if (select_width == 0) {
    return create_concat_lpm_data2(lpm, select_number);
  }
  else {
    unsigned id = new_id();
    unsigned width = ivl_lpm_width(lpm);
    unsigned select = id_of_nexus(ivl_lpm_select(lpm, select_width - 1));
    unsigned data_0 = create_mux(lpm, select_width - 1, select_number << 1);
    unsigned data_1 = create_mux(lpm, select_width - 1, (select_number << 1) | 1);
    indent();
    fprintf(output, "  (mux    %i %i %i %i %i)\n", id, width, select, data_0, data_1);
    return id;
  }
}







// Build the design hierarchy.
int build_hierarchy(ivl_scope_t scope, void* cd)
{
  int return_code;
  unsigned i, j;

  indent();
  fprintf(output, "(scope ");
  quoted_string(ivl_scope_tname(scope));
  fprintf(output, " ");
  quoted_string(ivl_scope_basename(scope));
  fprintf(output, " (\n");

  // Constants (root scope only)
  if (! level)
    for (i = 0; i < ivl_design_consts(design); i++) {
      ivl_net_const_t constant = ivl_design_const(design, i);
      const char* bits = ivl_const_bits(constant);
      unsigned pins = ivl_const_pins(constant);
      unsigned id = new_id();
      indent();
      fprintf(output, "  (const  %i \"", id);
      for (j = pins - 1; j < pins; j--)
        fprintf(output, "%c", bits[j]);
      fprintf(output, "\")\n");
      for (j = 0; j < pins; j++)
        create_bit_select(id_of_nexus(ivl_const_pin(constant, j)), pins, j, id);
    }

  // Parameters
  for (i = 0; i < ivl_scope_params(scope); i++) {
    ivl_parameter_t param = ivl_scope_param(scope, i);
    ivl_expr_t param_value = ivl_parameter_expr(param);
    unsigned width = ivl_expr_width(param_value);
    const char* bits;
    unsigned id = new_id();
    indent();
    fprintf(output, "  (const  %i \"", id);
    switch (ivl_expr_type(param_value)) {
      case IVL_EX_STRING : bits = ivl_expr_string(param_value); break;
      case IVL_EX_NUMBER : bits = ivl_expr_bits(param_value);   break;
      default            : fprintf(output, "** ERROR: Unknown parameter type."); return -1;
    }
    for (j = width - 1; j < width; j--)
      fprintf(output, "%c", bits[j]);
    fprintf(output, "\")\n");
    indent();
    fprintf(output, "  (name   %i ", new_id());
    quoted_string(ivl_parameter_basename(param));
    fprintf(output, " %i %i)\n", width, id);
  }

  // Signals
  for (i = 0; i < ivl_scope_sigs(scope); i++) {
    ivl_signal_t sig = ivl_scope_sig(scope, i);
    unsigned pins = ivl_signal_pins(sig);
    ivl_signal_port_t type = ivl_signal_port(sig);
    const char* name = ivl_signal_basename(sig);
    unsigned id;
    if (! level && type == IVL_SIP_INPUT) {
      id = new_id();
      fprintf(output, "  (input  %i \"%s\" %i)\n", id, name, pins);
      for (j = 0; j < pins; j++)
        create_bit_select(id_of_nexus(ivl_signal_pin(sig, j)), pins, j, id);
    }
    else if (! level && type == IVL_SIP_INOUT) {
      printf("** ERROR: Inout ports not supported.\n");
    }
    else if (! level && type == IVL_SIP_OUTPUT) {
      id = id_of_nexus(ivl_signal_pin(sig, 0));
      for (j = 1; j < pins; j++)
        id = create_bit_concat(id, j, ivl_signal_pin(sig, j));
      fprintf(output, "  (output %i \"%s\" %i %i)\n", new_id(), name, pins, id);
    }
    else {
      id = id_of_nexus(ivl_signal_pin(sig, 0));
      for (j = 1; j < pins; j++)
        id = create_bit_concat(id, j, ivl_signal_pin(sig, j));
      indent();
      fprintf(output, "  (name   %i ", new_id());  //XXX Why is "_s22" getting named?
      quoted_string(name);
      fprintf(output, " %i %i)\n", pins, id);
    }
  }

  // Logic
  for (i = 0; i < ivl_scope_logs(scope); i++) {
    unsigned id;
    ivl_net_logic_t log = ivl_scope_log(scope, i);
    switch (ivl_logic_type(log)) {
      case IVL_LO_BUF:
        indent();
        fprintf(output, "  (buf    %i 1 %i)\n", id_of_nexus(ivl_logic_pin(log, 0)), id_of_nexus(ivl_logic_pin(log, 1)));
        break;

      case IVL_LO_NOT:
        indent();
        fprintf(output, "  (not    %i 1 %i)\n", id_of_nexus(ivl_logic_pin(log, 0)), id_of_nexus(ivl_logic_pin(log, 1)));
        break;

      case IVL_LO_AND:
        indent();
        create_multi_gate("and ", id_of_nexus(ivl_logic_pin(log, 0)), log);
        break;
      
      case IVL_LO_NAND:
        id = new_id();
        indent();
        create_multi_gate("and ", id, log);
        indent();
        fprintf(output, "  (not    %i 1 %i)\n", id_of_nexus(ivl_logic_pin(log, 0)), id);
        break;
      
      case IVL_LO_XOR:
        indent();
        create_multi_gate("xor ", id_of_nexus(ivl_logic_pin(log, 0)), log);
        break;
      
      case IVL_LO_XNOR:
        id = new_id();
        indent();
        create_multi_gate("xor ", id, log);
        indent();
        fprintf(output, "  (not    %i 1 %i)\n", id_of_nexus(ivl_logic_pin(log, 0)), id);
        break;
      
      case IVL_LO_OR:
        indent();
        create_multi_gate("or  ", id_of_nexus(ivl_logic_pin(log, 0)), log);
        break;
      
      case IVL_LO_NOR:
        id = new_id();
        indent();
        create_multi_gate("or  ", id, log);
        indent();
        fprintf(output, "  (not    %i 1 %i)\n", id_of_nexus(ivl_logic_pin(log, 0)), id);
        break;

      default:
        printf("** ERROR: Unsupported logic type: %i.\n", ivl_logic_type(log));
        return -1;
    }
  }
  
  // LPMs
  for (i = 0; i < ivl_scope_lpms(scope); i++) {
    ivl_lpm_t lpm = ivl_scope_lpm(scope, i);
    ivl_lpm_type_t lpm_t = ivl_lpm_type(lpm);
    unsigned width   = ivl_lpm_width(lpm);
    unsigned selects;
    unsigned size;
    unsigned id, id1, id2, id3;
    switch (lpm_t) {
      case IVL_LPM_ADD:
        id  = new_id();
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        indent();
        fprintf(output, "  (add    %i %i %i %i)\n", id, width, id1, id2);
        create_split_lpm_q(lpm, id);
        break;

      case IVL_LPM_SUB:
        id  = new_id();
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        indent();
        fprintf(output, "  (sub    %i %i %i %i)\n", id, width, id1, id2);
        create_split_lpm_q(lpm, id);
        break;

      case IVL_LPM_MULT:
        id  = new_id();
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        indent();
        fprintf(output, "  (mul    %i %i %i %i)\n", id, width, id1, id2);
        create_split_lpm_q(lpm, id);
        break;

      case IVL_LPM_CMP_EQ:
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        indent();
        fprintf(output, "  (eq     %i %i %i %i)\n", id_of_nexus(ivl_lpm_q(lpm, 0)), width, id1, id2);
        break;

      case IVL_LPM_CMP_NE:
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        id  = new_id();
        indent();
        fprintf(output, "  (eq     %i %i %i %i)\n", id, width, id1, id2);
        indent();
        fprintf(output, "  (not    %i 1 %i)\n", id_of_nexus(ivl_lpm_q(lpm, 0)), id);
        break;

      case IVL_LPM_CMP_GT:
        // XXX Check for signed.
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        indent();
        fprintf(output, "  (lt     %i %i %i %i)\n", id_of_nexus(ivl_lpm_q(lpm, 0)), width, id2, id1);
        break;

      case IVL_LPM_CMP_GE:
        // XXX Check for signed.
        id1 = create_concat_lpm_data(lpm);
        id2 = create_concat_lpm_datab(lpm);
        id  = new_id();
        indent();
        fprintf(output, "  (lt     %i %i %i %i)\n", id, width, id1, id2);
        indent();
        fprintf(output, "  (not    %i 1 %i)\n", id_of_nexus(ivl_lpm_q(lpm, 0)), id);
        break;

      case IVL_LPM_FF:
        {
          ivl_nexus_t async_clr = ivl_lpm_async_clr(lpm);
          ivl_nexus_t async_set = ivl_lpm_async_set(lpm);
          ivl_nexus_t sync_clr = ivl_lpm_sync_clr(lpm);
          ivl_nexus_t sync_set = ivl_lpm_sync_set(lpm);
          ivl_nexus_t clk = ivl_lpm_clk(lpm);
          ivl_nexus_t enable = ivl_lpm_enable(lpm);
          if (async_set || sync_set) { perror("** ERROR: Does not support registers with async or sync sets.\n"); return -1; }
          id = new_id();
          id1 = create_concat_lpm_data(lpm);
          if (enable) {
            id2 = new_id();
            indent();
            fprintf(output, "  (mux    %i %i %i %i %i)\n", id2, width, id_of_nexus(enable), id, id1);
            id1 = id2;
          }
          if (sync_clr) {
            id2 = new_id();
            id3 = new_id();
            indent();
            fprintf(output, "  (const  %i \"", id3);
            for (j = 0; j < width; j++)
              fprintf(output, "0");
            fprintf(output, "\")\n");
            indent();
            fprintf(output, "  (mux    %i %i %i %i %i)\n", id2, width, id_of_nexus(sync_clr), id1, id3);
            id1 = id2;
          }
          // XXX Default to posedge sensitivity.
          if (async_clr) {
            indent();
            fprintf(output, "  (ffc    %i %i %i %i %i)\n", id, width, id_of_nexus(async_clr), id_of_nexus(clk), id1);
          }
          else {
            indent();
            fprintf(output, "  (ff     %i %i %i %i)\n", id, width, id_of_nexus(clk), id1);
          }
          create_split_lpm_q(lpm, id);
        }
        break;

      case IVL_LPM_MUX:
        {
          unsigned t = 1;
          selects = ivl_lpm_selects(lpm);
          size    = ivl_lpm_size(lpm);
          for (j = 0; j < selects; j++)
            t = t * 2;
          assert(t == size); // General case.
          id = create_mux(lpm, selects, 0);
          create_split_lpm_q(lpm, id);
        }
        break;

      default:
        perror("** ERROR: Unsupported LPM type.\n");
        return -1;
    }
  }
  level = level + 1;
  return_code = ivl_scope_children(scope, build_hierarchy, 0);
  level = level - 1;
  indent();
  fprintf(output, "))\n");
  return return_code;
}



/* Ivl entry point. */
int target_design(ivl_design_t des)
{
  design = des;
  output = fopen(ivl_design_flag(design, "-o"), "w");
  if (output == 0) {
    /*
    perror("** ERROR: Can not opening output file \"%s\".\n\n", ivl_design_flag(design, "-o"));
    */
    perror(ivl_design_flag(design, "-o"));
    return -1;
  }

  level = 0;
  build_hierarchy(ivl_design_root(design), 0);
  delete_nexus_table(nexus_table);

  design = 0;
  fclose(output);
  output = 0;
  return 0;
}


