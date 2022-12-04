CLASS zcl_day1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS:
      get_input_data RETURNING VALUE(rt_input) TYPE string_table,
      create_client
        IMPORTING url              TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_elfs_calories,
        elf_id TYPE i,
        total  TYPE i,
      END OF ty_s_elfs_calories,
      ty_t_elfs_calories TYPE TABLE OF ty_s_elfs_calories WITH KEY elf_id.

    METHODS:
      get_calories_per_elf
        IMPORTING it_input           TYPE string_table
        RETURNING VALUE(rt_calories) TYPE ty_t_elfs_calories,
      get_most_calories
        IMPORTING it_calories             TYPE ty_t_elfs_calories
        RETURNING VALUE(rv_most_calories) TYPE i,
      get_top_three
        IMPORTING it_calories                  TYPE ty_t_elfs_calories
        RETURNING VALUE(rv_top_three_calories) TYPE i.

ENDCLASS.


CLASS zcl_day1 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA(lt_input) = get_input_data( ).
    DATA(lt_calories) = get_calories_per_elf( lt_input ).
    DATA(lv_most_calories) = get_most_calories( lt_calories ).
    DATA(lv_top_three) = get_top_three( lt_calories ).

    out->write( |Most Calories is: { lv_most_calories } | ).
    out->write( |Total calories by the top three:  { lv_top_three } | ).

  ENDMETHOD.

  METHOD get_input_data.

    CONSTANTS lc_url TYPE string VALUE 'https://raw.githubusercontent.com/Daniel-luis/AdventOfCode2022/main/input_files/day1.txt'.
    DATA(lo_client) = create_client( lc_url ).
    DATA(ls_response) = lo_client->execute( if_web_http_client=>get )->get_text( ).
    lo_client->close( ).

    SPLIT ls_response AT cl_abap_char_utilities=>newline INTO TABLE rt_input.

  ENDMETHOD.


  METHOD create_client.

    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    ro_result = cl_web_http_client_manager=>create_by_http_destination( dest ).

  ENDMETHOD.

  METHOD get_calories_per_elf.



    DATA(lv_elf_id) = 1.
    LOOP AT it_input INTO DATA(ls_string_tab).
      IF ls_string_tab <> space.
        DATA(lv_tot_calories) += CONV i( ls_string_tab ).
      ELSE.
        APPEND VALUE ty_s_elfs_calories(
            elf_id = lv_elf_id
            total  = lv_tot_calories ) TO rt_calories.
        lv_elf_id += 1.
        CLEAR lv_tot_calories.
      ENDIF.
    ENDLOOP.

    SORT rt_calories BY total DESCENDING.

  ENDMETHOD.

  METHOD get_most_calories.

    rv_most_calories = it_calories[ 1 ]-total.

  ENDMETHOD.

  METHOD get_top_three.

    rv_top_three_calories = it_calories[ 1 ]-total + it_calories[ 2 ]-total + it_calories[ 3 ]-total.

  ENDMETHOD.

ENDCLASS.