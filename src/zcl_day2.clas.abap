CLASS zcl_day2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

    TYPES:
      BEGIN OF ty_s_results,
        opponent_choice TYPE char1,
        my_choice       TYPE char2,
        shape_score     TYPE i,
        round_outcome   TYPE i,
        total_points    TYPE i,
      END OF ty_s_results,
      ty_t_results TYPE TABLE OF ty_s_results WITH EMPTY KEY.

    METHODS:
      get_input_data RETURNING VALUE(rt_input) TYPE string_table,
      create_client
        IMPORTING url              TYPE string
        RETURNING VALUE(ro_result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check,
      calcaulate_results CHANGING ct_results TYPE ty_t_results,
      get_total_score
        IMPORTING it_results      TYPE ty_t_results
        RETURNING VALUE(rv_total) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS
      prepare_input
        IMPORTING it_input         TYPE string_table
        RETURNING VALUE(rt_scores) TYPE ty_t_results.

ENDCLASS.


CLASS zcl_day2 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA(lt_input) = get_input_data( ).
    DATA(lt_results) = prepare_input( lt_input ).
    calcaulate_results( CHANGING ct_results = lt_results ).

    out->write( |Total result is: { get_total_score( lt_results ) } | ).

  ENDMETHOD.

  METHOD get_input_data.

    CONSTANTS lc_url TYPE string VALUE 'https://raw.githubusercontent.com/Daniel-luis/AdventOfCode2022/main/input_files/day2.txt'.
    DATA(lo_client) = create_client( lc_url ).
    DATA(ls_response) = lo_client->execute( if_web_http_client=>get )->get_text( ).
    lo_client->close( ).

    SPLIT ls_response AT cl_abap_char_utilities=>newline INTO TABLE rt_input.

  ENDMETHOD.

  METHOD create_client.

    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    ro_result = cl_web_http_client_manager=>create_by_http_destination( dest ).

  ENDMETHOD.

  METHOD prepare_input.

    rt_scores = VALUE #( FOR work_area IN it_input
                             ( opponent_choice = COND #( WHEN work_area(1) = 'A' THEN 'R'
                                                         WHEN work_area(1) = 'B' THEN 'P'
                                                         WHEN work_area(1) = 'C' THEN 'S' )
                               my_choice = COND #( WHEN work_area+2(1) = 'X' THEN 'R'
                                                   WHEN work_area+2(1) = 'Y' THEN 'P'
                                                   WHEN work_area+2(1) = 'Z' THEN 'S' ) ) ).

  ENDMETHOD.

  METHOD calcaulate_results.

    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<ls_results>).

      <ls_results>-shape_score = COND i( WHEN <ls_results>-my_choice = 'R' THEN 1
                                         WHEN <ls_results>-my_choice = 'P' THEN 2
                                         WHEN <ls_results>-my_choice = 'S' THEN 3 ).

      "Draw
      IF <ls_results>-my_choice = <ls_results>-opponent_choice.
        <ls_results>-round_outcome = 3.
        "Victory
      ELSEIF <ls_results>-my_choice = 'R' AND <ls_results>-opponent_choice = 'S' OR
          <ls_results>-my_choice = 'P' AND <ls_results>-opponent_choice = 'R' OR
          <ls_results>-my_choice = 'S' AND <ls_results>-opponent_choice = 'P'.
        <ls_results>-round_outcome = 6.
        "Loss
      ELSE.
        <ls_results>-round_outcome = 0.
      ENDIF.

      <ls_results>-total_points = <ls_results>-shape_score + <ls_results>-round_outcome.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_total_score.

    rv_total = REDUCE i( INIT x = 0
                         FOR ls_results IN it_results
                         NEXT x += ls_results-total_points ).

  ENDMETHOD.

ENDCLASS.