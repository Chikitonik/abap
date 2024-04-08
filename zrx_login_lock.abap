
*extra felds in SQ02:
*ADRP_NAME_TEXT
*TASK_NUMBER
*USR02_GLTGB
*USR21_BNAME
*ZSD_IM_USERS_DATET
*ZSD_IM_USERS_LOGIN
*ZSD_IM_USERS_TABNUM
*&---------------------------------------------------------------------*
*& Code in 2 start-of-selection 
*&---------------------------------------------------------------------*
*& Get data from selection screen
*&---------------------------------------------------------------------*
DATA:
  gt_view                        TYPE ztbpf_base_rsparams,   " z type is already in the system
  ls_view                        LIKE LINE OF gt_view,
  lv_persnumber                  TYPE adrp-persnumber,
  lv_count_persnumber            TYPE i,
  lv_zsd_im_users_login          TYPE zsd_im_users-login,
  lv_answer                      TYPE sy-ucomm,
  lv_tabnumber                   TYPE zsd_im_users-tabnumber,
  lv_count_tabnumber             TYPE i,
  lv_is_login_ETP_found          TYPE boolean, 
  lv_is_login_SAP_found          TYPE boolean, 
  lv_is_login_ETP_already_locked TYPE boolean, 
  lv_is_login_SAP_already_locked TYPE boolean, 
  lt_old_login_data              TYPE TABLE OF zsd_im_users,
  ls_old_login_data              LIKE LINE OF lt_old_login_data,
  ls_new_login_data              LIKE LINE OF lt_old_login_data,
  lv_objectid                    TYPE cdhdr-objectid,
  lt_icdtxt_zsd_im_users         TYPE TABLE OF cdtxt,
  lt_result_bapi_user_lock       TYPE STANDARD TABLE OF bapiret2,
  ls_result_bapi_user_lock       LIKE LINE OF lt_result_bapi_user_lock,
  ls_bapilogond                  TYPE bapilogond,
  ls_bapilogonx                  TYPE bapilogonx,
  lv_text                        TYPE string,
  lt_txline                      TYPE STANDARD TABLE OF txline,
  lv_textline1                   TYPE string,
  lv_textline2                   TYPE string,
  lv_textline3                   TYPE string.

CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
  EXPORTING
    curr_report     = sy-repid
  TABLES
    selection_table = gt_view
  EXCEPTIONS
    not_found       = 1
    no_report       = 2
    OTHERS          = 3.

IF sy-subrc NE 0.
  MESSAGE 'Incorrect selection screen data' TYPE 'I'.
  RETURN.
ENDIF.
* get tabnumber
READ TABLE gt_view INTO ls_view WITH KEY SELNAME = 'SP$00001'." needed to check/correct  
lv_tabnumber = ls_view-low.
READ TABLE gt_view INTO ls_view WITH KEY SELNAME = 'SP$00003'." needed to check/correct 
TASK_NUMBER = ls_view-low.

IF STRLEN( lv_tabnumber ) < 11.
  MESSAGE 'Не верный табельный номер введен' TYPE 'I'.
  RETURN.
ENDIF.

ZSD_IM_USERS_TABNUM = lv_tabnumber.
* get and set login
SELECT COUNT( * ) INTO lv_count_tabnumber FROM zsd_im_users WHERE TABNUMBER = lv_tabnumber.
IF lv_count_tabnumber < 2.
  SELECT SINGLE login INTO lv_zsd_im_users_login FROM zsd_im_users WHERE TABNUMBER = lv_tabnumber.
  SELECT SINGLE DATET INTO ZSD_IM_USERS_DATET FROM zsd_im_users WHERE TABNUMBER = lv_tabnumber.
  IF sy-subrc NE 0.
    lv_zsd_im_users_login = 'нет логина ЭТП'.
    lv_text = 'Логина ЭТП нет'.
    INSERT lv_text INTO lt_txline INDEX 1.
  ELSE.
    ZSD_IM_USERS_LOGIN = lv_zsd_im_users_login.
    lv_is_login_ETP_found = abap_true.
    IF ZSD_IM_USERS_DATET <= sy-datum.
      lv_is_login_ETP_already_locked = abap_true.
    ENDIF.
  ENDIF.
ELSE.
  MESSAGE 'Найдено более одного такого табельного в таблице zsd_im_users, проверьте вручную' TYPE 'I'.
  RETURN.
ENDIF.
* get persnumber, set login SAP and name
SELECT COUNT( * ) INTO lv_count_persnumber FROM ADRP WHERE SORT2 = lv_tabnumber.
IF lv_count_persnumber < 2.
  SELECT SINGLE persnumber INTO lv_persnumber FROM ADRP WHERE SORT2 = lv_tabnumber.
    IF sy-subrc NE 0.
      USR21_BNAME = 'нет логина SAP'.
      ADRP_NAME_TEXT = 'нет логина SAP'.
      lv_text = 'Логина SAP нет'.
      INSERT lv_text INTO lt_txline INDEX 1.
    ELSE.
      SELECT SINGLE BNAME INTO USR21_BNAME FROM USR21 WHERE persnumber = lv_persnumber.
      SELECT SINGLE NAME_TEXT INTO ADRP_NAME_TEXT FROM ADRP WHERE SORT2 = lv_tabnumber.
      SELECT SINGLE GLTGB INTO USR02_GLTGB FROM USR02 WHERE BNAME = USR21_BNAME.
      lv_is_login_SAP_found = abap_true.
      IF USR02_GLTGB <= sy-datum.
        lv_is_login_SAP_already_locked = abap_true.
      ENDIF.
  ENDIF.
  ELSE.
    MESSAGE 'Найдено более одного такого табельного в таблице ADRP, проверьте вручную' TYPE 'I'.
    RETURN.
ENDIF.

ls_new_login_data = ls_old_login_data.
CONCATENATE sy-mandt ls_new_login_data-login INTO lv_objectid.
ls_new_login_data-PASSWORD = |* deleted { TASK_NUMBER } *|.
ls_new_login_data-DATET = sy-datum.
* text for popup
IF lv_is_login_ETP_found = abap_true.
  lv_textline1 = |Найден логин ЭТП { ZSD_IM_USERS_LOGIN } |.
  IF lv_is_login_ETP_already_locked = abap_true.
    lv_textline1 = |Логин ЭТП { ZSD_IM_USERS_LOGIN } уже блокирован ранее|.
    INSERT lv_textline1 INTO lt_txline INDEX 1.
  ENDIF.
  ELSE.
    lv_textline1 = |Логин ЭТП не найден |.
ENDIF.
IF lv_is_login_SAP_found = abap_true.
  lv_textline2 = |Найден логин SAP { USR21_BNAME } |.
  IF lv_is_login_SAP_already_locked = abap_true.
    lv_textline2 = |Логин SAP { USR21_BNAME } уже блокирован ранее|.
    INSERT lv_textline2 INTO lt_txline INDEX 1.
  ENDIF.
  ELSE.
    lv_textline2 = |Логин SAP не найден |.
ENDIF.
lv_textline3 = |Будет установлен пароль { ls_new_login_data-PASSWORD } и дата "по" сегодня|.
* show popup
CALL FUNCTION 'POPUP_TO_DECIDE'
  EXPORTING
    titel           = |Табельный { lv_tabnumber }|
    textline1       = lv_textline1
    TEXTLINE2       = lv_textline2
    TEXTLINE3       = lv_textline3
    TEXT_OPTION1    = 'БЛОКИРОВАТЬ'
    TEXT_OPTION2    = 'Cancel'
    CANCEL_DISPLAY  = ''
    ICON_TEXT_OPTION1 = 'ICON_MESSAGE_ERROR_SMALL'
  IMPORTING
    answer          = lv_answer
  EXCEPTIONS
    text_not_found  = 1
    OTHERS          = 2.
*&---------------------------------------------------------------------*
*& blocking
*&---------------------------------------------------------------------*
IF sy-subrc = 0 AND lv_answer = '1'.   " 1=Confirm, 2=Cancel
* user ETP
  IF lv_is_login_ETP_found = abap_true AND lv_is_login_ETP_already_locked <> abap_true.
    SELECT * FROM zsd_im_users INTO CORRESPONDING FIELDS OF TABLE lt_old_login_data
    WHERE login = ZSD_IM_USERS_LOGIN.
      IF sy-subrc = 0.
        READ TABLE lt_old_login_data INTO ls_old_login_data WITH KEY login = ZSD_IM_USERS_LOGIN.
        ELSE.
          MESSAGE 'No data found' TYPE 'I'.
          RETURN.
      ENDIF. 
* for the history in RSSCD100
    CALL FUNCTION 'ZSD_IM_USERS_WRITE_DOCUMENT' 
    EXPORTING
      objectid            = lv_objectid "example 102RKHOLODKOV@ARMTEK.RU
      tcode               = 'ZLOGIN'
      utime               = sy-uzeit
      udate               = sy-datum
      username            = sy-uname
      n_zsd_im_users      = ls_new_login_data
      o_zsd_im_users      = ls_old_login_data
      upd_zsd_im_users    = 'U'
    TABLES
      icdtxt_zsd_im_users = lt_icdtxt_zsd_im_users. "проверить что выдает и грамотно обработать
* blocking login ETP in Z table
    UPDATE ZSD_IM_USERS SET
      PASSWORD = @ls_new_login_data-PASSWORD,
      DATET   = @sy-datum
    WHERE login = @ZSD_IM_USERS_LOGIN.
* message if was blocked
    IF sy-subrc = 0.
      lv_text = |Логин ЭТП { ZSD_IM_USERS_LOGIN } блокирован |.
      INSERT lv_text INTO lt_txline INDEX 1.
    ELSE.
      lv_text = |Логин ЭТП { ZSD_IM_USERS_LOGIN } не был блокирован, проверьте вручную |.
      INSERT lv_text INTO lt_txline INDEX 1.
    ENDIF.
  ELSE.
  ENDIF.
* user SAP
  ls_bapilogond-GLTGB = sy-datum.
  ls_bapilogonx-GLTGB = 'X'.
  IF lv_is_login_SAP_found = abap_true AND lv_is_login_SAP_already_locked <> abap_true.
    CALL FUNCTION 'BAPI_USER_LOCK'
      EXPORTING 
        USERNAME    = USR21_BNAME
      TABLES
        return      = lt_result_bapi_user_lock.
    READ TABLE lt_result_bapi_user_lock INTO ls_result_bapi_user_lock INDEX 1.
    IF sy-subrc = 0.
        IF ls_result_bapi_user_lock-TYPE <> 'S'.
        MESSAGE ls_result_bapi_user_lock-MESSAGE TYPE 'I'.
        RETURN.
        ENDIF.  
      ENDIF.  
    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING 
        USERNAME    = USR21_BNAME
        LOGONDATA   = ls_bapilogond
        LOGONDATAX  = ls_bapilogonx
      TABLES
        return   = lt_result_bapi_user_lock.
    READ TABLE lt_result_bapi_user_lock INTO ls_result_bapi_user_lock INDEX 1.
    IF sy-subrc = 0.
      IF ls_result_bapi_user_lock-TYPE <> 'S'.
        MESSAGE ls_result_bapi_user_lock-MESSAGE TYPE 'I'.
        RETURN.
      ENDIF.  
    ENDIF.  
    lv_text = |Логин SAP { USR21_BNAME } блокирован |.
    INSERT lv_text INTO lt_txline INDEX 1.
  ELSE.
  ENDIF.
* message about results (with posibility to quick copy)
  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR' "text window
  EXPORTING
    im_title        = 'Для копирования в наряд'
    im_display_mode = 'X'
  CHANGING
    ch_text         = lt_txline[].
  ELSE.
* message if canceled
  MESSAGE 'Отменено' TYPE 'S'.
ENDIF.

* report's fields updating
SELECT SINGLE DATET INTO ZSD_IM_USERS_DATET FROM zsd_im_users WHERE TABNUMBER = lv_tabnumber.
SELECT SINGLE GLTGB INTO USR02_GLTGB FROM USR02 WHERE BNAME = USR21_BNAME.