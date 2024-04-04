*2 start-of-selection
*&---------------------------------------------------------------------*
*& Get data from selection screen
*&---------------------------------------------------------------------*
DATA:
  gt_view TYPE ztbpf_base_rsparams,   " z type is already in the system
  ls_view LIKE LINE OF gt_view,
  lv_persnumber TYPE adrp-persnumber,
  lv_count_persnumber TYPE i,
  lv_zsd_im_users_login TYPE zsd_im_users-login,
  lv_answer TYPE sy-ucomm,
  lv_count_tabnumber TYPE i,
  lv_tabnumber TYPE zsd_im_users-tabnumber,
  lt_old_login_data TYPE TABLE OF zsd_im_users,
  ls_old_login_data LIKE LINE OF lt_old_login_data,
  ls_new_login_data LIKE LINE OF lt_old_login_data,
  lv_objectid TYPE cdhdr-objectid,
  lt_icdtxt_zsd_im_users TYPE TABLE OF cdtxt,
  lv_text TYPE string,
  lt_txline  TYPE STANDARD TABLE OF txline,
  lv_textline1 TYPE string,
  lv_textline2 TYPE string,
  lv_textline3 TYPE string.

CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
  EXPORTING
    curr_report     = sy-repid
  TABLES
    selection_table = lt_selection_params
  EXCEPTIONS
    not_found       = 1
    no_report       = 2
    OTHERS          = 3.

IF sy-subrc NE 0.
  MESSAGE 'Incorrect selection screen data' TYPE 'I'.
  RETURN.
ENDIF.

* get tabnumber
READ TABLE gt_view INTO ls_view WITH KEY SELNAME = 'SP$00001'.
lv_tabnumber = ls_view-low.

IF STRLEN( lv_tabnumber ) < 11.
  MESSAGE 'Не верный номер введен' TYPE 'I'.
  RETURN.
ENDIF.

ZSD_IM_USERS_TABNUM = lv_tabnumber.

* get and set login
SELECT COUNT( * ) INTO lv_count_tabnumber FROM zsd_im_users WHERE TABNUMBER = lv_tabnumber.
IF lv_count_tabnumber < 2.
  SELECT SINGLE login INTO lv_zsd_im_users_login FROM zsd_im_users WHERE TABNUMBER = lv_tabnumber.
  IF sy-subrc NE 0.
    lv_zsd_im_users_login = 'no login'.
  ELSE.
    ZSD_IM_USERS_LOGIN = lv_zsd_im_users_login.
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
        USR21_BNAME = 'no login SAP'.
        ADRP_NAME_TEXT = 'no data'.
    ELSE.
        SELECT SINGLE BNAME INTO USR21_BNAME FROM USR21 WHERE persnumber = lv_persnumber.
        SELECT SINGLE NAME_TEXT INTO ADRP_NAME_TEXT FROM ADRP WHERE SORT2 = lv_tabnumber.
    ENDIF.
  ELSE.
    MESSAGE 'Найдено более одного такого табельного в таблице ADRP, проверьте вручную' TYPE 'I'.
    RETURN.
ENDIF.
* text for popup
lv_textline1 = |Найден логин ЭТП: { ZSD_IM_USERS_LOGIN } |.
lv_textline2 = |Найден логин SAP: { USR21_BNAME } |.
lv_textline3 = 'Будет установлен пароль *deleted 0123456789* и дата "по" сегодня'.
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
* block
IF sy-subrc = 0 AND lv_answer = '1'.   " 1=Confirm, 2=Cancel
*****проверить если дата по уже сегодня или меньше
*****************************************************
*&---------------------------------------------------------------------*
*& blocking
*&---------------------------------------------------------------------*
  if ZSD_IM_USERS_LOGIN <> 'no login'.

    SELECT * FROM zsd_im_users INTO CORRESPONDING FIELDS OF TABLE lt_old_login_data
    WHERE login = ZSD_IM_USERS_LOGIN.

      IF sy-subrc = 0.
        READ TABLE lt_old_login_data INTO ls_old_login_data WITH KEY login = ZSD_IM_USERS_LOGIN.
        ELSE.
          MESSAGE 'No data found' TYPE 'I'.
          RETURN.
      ENDIF. 

    ls_new_login_data = ls_old_login_data.

    CONCATENATE sy-mandt ls_new_login_data-login INTO objectid.
    ls_new_login_data-PASSWORD = '0123456789'.
    ls_new_login_data-DATET = sy-datum.

* for the history in RSSCD100
    CALL FUNCTION 'ZSD_IM_USERS_WRITE_DOCUMENT' 
    EXPORTING
      objectid            = objectid "example 102RKHOLODKOV@ARMTEK.RU
      tcode               = 'ZLOGIN'
      utime               = sy-uzeit
      udate               = sy-datum
      username            = sy-uname
      n_zsd_im_users      = ls_new_login_data
      o_zsd_im_users      = ls_old_login_data
      upd_zsd_im_users    = 'U'
    TABLES
      icdtxt_zsd_im_users = icdtxt_zsd_im_users.

* blocking login ETP
    UPDATE ZSD_IM_USERS SET
      PASSWORD = @ls_new_login_data-PASSWORD,
      DATET   = @sy-datum
    WHERE login = @ZSD_IM_USERS_LOGIN.

* message if was blocked
    lv_text = |Логин ЭТП { ZSD_IM_USERS_LOGIN } блокирован |.
    INSERT lv_text INTO li_txline INDEX 1.
    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR' "text window
    EXPORTING
      im_title        = 'Для копирования в наряд'
      im_display_mode = 'X'
    CHANGING
      ch_text         = li_txline[].
    ENDIF.
ELSE.
* message if canceled
  MESSAGE 'Отменено' TYPE 'S'.
ENDIF.
