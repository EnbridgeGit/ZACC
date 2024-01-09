FUNCTION z_acc_gpf_user_exit_brazil .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATA_IN) TYPE  ZACC_EXIT_DATA_IN
*"     VALUE(LOG_HANDLE) TYPE  BALLOGHNDL OPTIONAL
*"  CHANGING
*"     REFERENCE(DATA_OUT) TYPE  ZACC_EXIT_DATA_OUT
*"  EXCEPTIONS
*"      SKIP_BANK_ENTRY
*"----------------------------------------------------------------------

* Sample User Exit for Brazilian banks
* Put branch code from local clearing code into Branch field
* Branch code is expected to be the last 4 digits of the clearing code

  DATA: v_len TYPE i,
        v_pos TYPE i,
        v_code(15) TYPE c.

* Verify bank entry load mode: only activate on Add or Modify
  CHECK data_in-action CA 'AM'.
* Copy last 4 digits from local clearing code to bank branch
  v_code = data_in-natid.
  v_len = STRLEN( v_code ).
  v_pos = v_len - 4.
  IF v_pos > 0.
    data_out-branch = v_code+v_pos(4).
  ENDIF.

ENDFUNCTION.
