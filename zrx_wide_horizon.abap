*2 start-of-selection
*&---------------------------------------------------------------------*
*& Get data from selection screen
*&---------------------------------------------------------------------*
DATA:
  gt_view TYPE ztbpf_base_rsparams,   " z type is already in the system
  ls_view LIKE LINE OF gt_view,
  lv_vbeln TYPE VBAK-VBELN,
  lv_vkorg TYPE VBAK-VKORG,
  lv_adays TYPE ZSD_DAYS-ADAYS,
  lv_zzdatesch TYPE VBAP-ZZDATESCH,
  lv_delta TYPE STRING,
  lv_text TYPE STRING,
  li_txline  TYPE STANDARD TABLE OF txline,
  lv_TEXTLINE3 TYPE STRING,
  lv_vbobj TYPE VBUK-VBOBJ.
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
* Get order number
READ TABLE gt_view INTO ls_view WITH KEY SELNAME = 'SP$00001'.
lv_vbeln = ls_view-LOW.
* Check order number
SELECT SINGLE VBOBJ INTO lv_vbobj FROM VBUK WHERE VBELN = lv_VBELN.
IF lv_vbobj <> 'A'.
  MESSAGE 'Веден не номер сбытового заказа' TYPE 'I'.
  EXIT.
ENDIF.
*&---------------------------------------------------------------------*
*& getting additional information
*&---------------------------------------------------------------------*
SELECT SINGLE VKORG INTO lv_vkorg FROM VBAK WHERE VBELN = lv_VBELN.
SELECT SINGLE ADAYS INTO lv_adays FROM ZSD_DAYS WHERE VKORG = lv_vkorg and TYPE = 'H'.
SELECT SINGLE ZZDATESCH INTO lv_zzdatesch FROM VBAP WHERE VBELN = lv_VBELN.
lv_delta = sy-datum - lv_zzdatesch - lv_adays.
IF lv_delta > 0.
  lv_TEXTLINE3 = |Превышение горизонта в данном заказе { lv_delta } дней|.
ELSE.
  lv_TEXTLINE3 = icon_message_error_small && 'Превышения горизонта нет!'.
ENDIF.
*&---------------------------------------------------------------------*
*& Update VBAP
*&---------------------------------------------------------------------*
DATA:
  lv_answer TYPE sy-ucomm.
CALL FUNCTION 'POPUP_TO_DECIDE'
  EXPORTING
    titel           = 'Подтверждение'
    textline1       = 'Обновить даты в заказе (будут изменены на вчерашнее число)?'
    TEXTLINE2       = |Сбытовая в заказе { lv_vkorg }, для нее настройка горизонта { lv_adays } дней|
    TEXTLINE3       = lv_TEXTLINE3
    TEXT_OPTION1    = 'Update Dates'
    TEXT_OPTION2    = 'Cancel'
    CANCEL_DISPLAY  = ''
    ICON_TEXT_OPTION1 = 'ICON_MESSAGE_ERROR_SMALL'
  IMPORTING
    answer          = lv_answer
  EXCEPTIONS
    text_not_found  = 1
    OTHERS          = 2.
IF sy-subrc = 0 AND lv_answer = '1'.   " 1=Confirm, 2=Cancel
  SUBTRACT 1 FROM sy-datum.
  UPDATE VBAP SET
    ZZDATESCH = @sy-datum,
    ZZCLDAT   = @sy-datum,
    ZZKODAT   = @sy-datum
  WHERE vbeln = @lv_vbeln.
  lv_text = |Для заказа { lv_vbeln } горизонт расширен|.
  INSERT lv_text INTO li_txline INDEX 1.
  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR' "text window
  EXPORTING
   im_title        = 'Для копирования в наряд'
   im_display_mode = 'X'
  CHANGING
  ch_text         = li_txline[].
ELSE.
  MESSAGE 'Отменено' TYPE 'S'.
ENDIF.