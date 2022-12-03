CLASS zcl_hello_world DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS hello RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.

CLASS zcl_hello_world IMPLEMENTATION.

  METHOD hello.
    rv_result = 'Hello, world!'.
  ENDMETHOD.

ENDCLASS.