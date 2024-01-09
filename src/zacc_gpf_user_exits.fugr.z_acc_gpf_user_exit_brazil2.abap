FUNCTION Z_ACC_GPF_USER_EXIT_BRAZIL2 .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATA_IN) TYPE  ZACC_EXIT_DATA_IN
*"     VALUE(LOG_HANDLE) TYPE  BALLOGHNDL OPTIONAL
*"  CHANGING
*"     REFERENCE(DATA_OUT) TYPE  ZACC_EXIT_DATA_OUT
*"  EXCEPTIONS
*"      SKIP_BANK_ENTRY
*"--------------------------------------------------------------------

* Sample User Exit for Brazilian banks
* Put branch code from local clearing code into Branch field
* Branch code is expected to be in positions 4-7 of the clearing code

  DATA: v_len TYPE i,
        v_pos TYPE i,
        v_code(15) TYPE c.

* Verify bank entry load mode: only activate on Add or Modify
  CHECK data_in-action CA 'AM'.
* Check the code is at least 7 digits long, so this makes sense
  v_code = data_in-natid.
  v_len = STRLEN( v_code ).
  CHECK v_len >= 7.
* Copy positions 4-7 to the branch name
  data_out-branch = v_code+3(4).

ENDFUNCTION.
