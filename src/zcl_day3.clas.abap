CLASS zcl_day3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      BEGIN OF
        ty_s_rucksack,
        original           TYPE string,
        first_compartment  TYPE string,
        second_compartment TYPE string,
        repeated_item      TYPE char1,
        priority           TYPE i,
      END OF ty_s_rucksack,
      ty_t_rucksack TYPE TABLE OF ty_s_rucksack WITH EMPTY KEY,

      BEGIN OF ty_s_priorities,
        item     TYPE char1,
        priority TYPE i,
      END OF ty_s_priorities,
      ty_t_priorities TYPE SORTED TABLE OF ty_s_priorities WITH UNIQUE KEY item.


    METHODS:
      get_input_data RETURNING VALUE(rt_input) TYPE string_table,
      create_client
        IMPORTING url              TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check,
      prepare_input
        IMPORTING it_input           TYPE string_table
        RETURNING VALUE(rt_rucksack) TYPE ty_t_rucksack,
      get_total_score
        IMPORTING it_rucksack     TYPE ty_t_rucksack
        RETURNING VALUE(rv_total) TYPE i      .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      get_repeated_item
        IMPORTING iv_input_length       TYPE i
                  iv_first_compartment  TYPE ty_s_rucksack-first_compartment
                  iv_second_compartment TYPE ty_s_rucksack-second_compartment
        RETURNING VALUE(rv_item)        TYPE char1,

      set_priorities,

      get_priority
        IMPORTING iv_item            TYPE char1
        RETURNING VALUE(rv_priority) TYPE ty_s_priorities-priority.

    DATA gt_priorities TYPE ty_t_priorities.
ENDCLASS.



CLASS zcl_day3 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    set_priorities( ).
    DATA(lt_input) = get_input_data( ).
    DATA(lt_rucksack) = prepare_input( lt_input ).

    out->write( |Sum of priorities is: { get_total_score( lt_rucksack ) } | ).

  ENDMETHOD.

  METHOD get_input_data.

    CONSTANTS: lc_url TYPE string VALUE 'https://raw.githubusercontent.com/Daniel-luis/AdventOfCode2022/main/input_files/day3.txt'.
    DATA(lo_client) = create_client( lc_url ).
    DATA(ls_response) = lo_client->execute( if_web_http_client=>get )->get_text(  ).
    lo_client->close( ).

    SPLIT ls_response AT cl_abap_char_utilities=>newline INTO TABLE rt_input.

  ENDMETHOD.

  METHOD create_client.

    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    ro_result = cl_web_http_client_manager=>create_by_http_destination( dest ).

  ENDMETHOD.

  METHOD prepare_input.

    DATA: ls_rucksack LIKE LINE OF rt_rucksack.
    LOOP AT it_input INTO DATA(ls_input).
      DATA(lv_input_length) = numofchar( ls_input ).
      ls_rucksack-original = ls_input.
      DATA(lv_compartment_length) = lv_input_length / 2.
      ls_rucksack-first_compartment = ls_input+0(lv_compartment_length).
      ls_rucksack-second_compartment = ls_input+lv_compartment_length.

      ls_rucksack-repeated_item = get_repeated_item( iv_input_length = lv_compartment_length
                                                     iv_first_compartment        = ls_rucksack-first_compartment
                                                     iv_second_compartment = ls_rucksack-second_compartment ).

      ls_rucksack-priority = get_priority( ls_rucksack-repeated_item ).

      APPEND ls_rucksack TO rt_rucksack.
      CLEAR ls_rucksack.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_repeated_item.

    CHECK iv_input_length > 0.
    DO iv_input_length TIMES.
      DATA(lv_current_position) = sy-index - 1.
      FIND FIRST OCCURRENCE OF iv_first_compartment+lv_current_position(1) IN iv_second_compartment.
      IF sy-subrc = 0.
        rv_item = iv_first_compartment+lv_current_position(1).
        RETURN.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD set_priorities.

    gt_priorities = VALUE #( ( item = |a| priority = 1 )  ( item = |b| priority = 2 )
                             ( item = |c| priority = 3 )  ( item = |d| priority = 4 )
                             ( item = |e| priority = 5 )  ( item = |f| priority = 6 )
                             ( item = |g| priority = 7 )  ( item = |h| priority = 8 )
                             ( item = |i| priority = 9 )  ( item = |j| priority = 10 )
                             ( item = |k| priority = 11 ) ( item = |l| priority = 12 )
                             ( item = |m| priority = 13 ) ( item = |n| priority = 14 )
                             ( item = |o| priority = 15 ) ( item = |p| priority = 16 )
                             ( item = |q| priority = 17 ) ( item = |r| priority = 18 )
                             ( item = |s| priority = 19 ) ( item = |t| priority = 20 )
                             ( item = |u| priority = 21 ) ( item = |v| priority = 22 )
                             ( item = |w| priority = 23 ) ( item = |x| priority = 24 )
                             ( item = |y| priority = 25 ) ( item = |z| priority = 26 )
                             ( item = |A| priority = 27 ) ( item = |B| priority = 28 )
                             ( item = |C| priority = 29 ) ( item = |D| priority = 30 )
                             ( item = |E| priority = 31 ) ( item = |F| priority = 32 )
                             ( item = |G| priority = 33 ) ( item = |H| priority = 34 )
                             ( item = |I| priority = 35 ) ( item = |J| priority = 36 )
                             ( item = |K| priority = 37 ) ( item = |L| priority = 38 )
                             ( item = |M| priority = 39 ) ( item = |N| priority = 40 )
                             ( item = |O| priority = 41 ) ( item = |P| priority = 42 )
                             ( item = |Q| priority = 43 ) ( item = |R| priority = 44 )
                             ( item = |S| priority = 45 ) ( item = |T| priority = 46 )
                             ( item = |U| priority = 47 ) ( item = |V| priority = 48 )
                             ( item = |W| priority = 49 ) ( item = |X| priority = 50 )
                             ( item = |Y| priority = 51 ) ( item = |Z| priority = 52 ) ).

  ENDMETHOD.

  METHOD get_priority.

    rv_priority = gt_priorities[ item = iv_item ]-priority.

  ENDMETHOD.

  METHOD get_total_score.

    rv_total = REDUCE i( INIT x = 0
                     FOR ls_rucksack IN it_rucksack
                     NEXT x += ls_rucksack-priority ).

  ENDMETHOD.

ENDCLASS.