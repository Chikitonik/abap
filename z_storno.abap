*************get data from selection screen***************
**********************************************************
DATA: gt_view TYPE ztbpf_base_rsparams. "z type is already in the sestem
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
    MESSAGE 'Не верно заполнен селекционный экран' TYPE 'I'.
    RETURN.
  ENDIF.
"get invoice number
DATA: lv_VBELN TYPE VBRP-VBELN,
      ls_view LIKE LINE OF gt_view.
READ TABLE gt_view INTO ls_view WITH KEY SELNAME
 = 'SP$00001'.
lv_VBELN = ls_view-LOW."invoice number
**************check invoice number************************
**********************************************************
DATA: lv_VBOBJ TYPE VBUK-VBOBJ.
SELECT SINGLE VBOBJ INTO lv_VBOBJ FROM VBUK
  WHERE VBELN = lv_VBELN.
    IF lv_VBOBJ <> 'F'.
       MESSAGE 'Введен не номер фактуры' TYPE 'I'.
       EXIT.
    ENDIF.
******check that there is no Cancel Invoice in flow******
*****check that invoice number is not Cancel Invoice*****
******check Invoise date*********************************
******check it is return*********************************
*********************************************************
DATA: lv_FKSTO TYPE VBRK-FKSTO,
  lv_VBTYP TYPE VBRK-VBTYP,
  lv_ERDAT TYPE VBRK-ERDAT,
  lv_FKART TYPE FKART.
SELECT SINGLE FKSTO VBTYP ERDAT FKART
  FROM VBRK
  INTO (lv_FKSTO, lv_VBTYP, lv_ERDAT, lv_FKART)
  WHERE VBELN = lv_VBELN.
lv_ERDAT = lv_ERDAT + 60.
IF lv_FKSTO = 'X'.
  MESSAGE 'Фактура уже сторнирована' TYPE 'I'.
  EXIT.
ELSEIF lv_VBTYP = 'N'.
  MESSAGE 'Введен номер сторнирующей фактуры' TYPE 'I'.
  EXIT.
ELSEIF lv_ERDAT < sy-datum.
  MESSAGE 'Обратите внимание, дата фактуры более 2х месяцев назад' TYPE 'I'.
ENDIF.
*********check for EDI***********************************
*********************************************************
DATA lv_clarIF_status TYPE  j_3rf_reginv_out-clarIF_status.
DATA lv_doc_status    TYPE  j_3rf_reginv_out-doc_status.
SELECT SINGLE doc_status clarIF_status
  FROM j_3rf_reginv_out
    INTO (lv_doc_status, lv_clarIF_status)
      WHERE vbeln_bdoc = lv_VBELN.
      IF sy-subrc = 0.
        IF ( lv_clarIF_status IS INITIAL AND lv_doc_status = 'CR') or "j3rdx_clar_note_created
           ( lv_clarIF_status IS INITIAL AND lv_doc_status = 'SG') or "j3rdx_clar_note_signed
           lv_clarIF_status = 'AN' or "Annulment created
           lv_clarIF_status = 'AC' or "Annulment Confirmed
           lv_clarIF_status = 'HD'. "ClarIFication Handled Уточнение обработано
          sy-subrc = 0.
        ELSE.
           MESSAGE i000(zsdsoft) WITH 'Фактура выгружена в ЭДО. Необходимо анулирование' DISPLAY LIKE IF_msg_output=>msgtype_error.
           sy-subrc = 4.
           EXIT.
        ENDIF.
      ELSE.
        sy-subrc = 0.
      ENDIF.
*********check for returns*******************************
*********************************************************
data:  begin of lt_VBELN_RE occurs 1, "returns table
  VBELN type VBRP-VBELN,
    end of lt_VBELN_RE.
SELECT VBELN INTO lt_VBELN_RE FROM VBFA
  WHERE VBELV = lv_VBELN and VBTYP_N = 'H'. "returns
  INSERT lt_VBELN_RE INTO TABLE lt_VBELN_RE.
  ENDSELECT.
data: lv_ABSTK type VBUK-ABSTK.
LOOP AT lt_VBELN_RE.
SELECT SINGLE ABSTK INTO lv_ABSTK FROM VBUK
  WHERE VBELN = lt_VBELN_RE-VBELN.
    IF lv_ABSTK  = ''.
    ELSE.
      IF lv_ABSTK <> 'C'.
        MESSAGE 'Есть не сторнированный возврат!' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.
ENDLOOP.
*************check for EWM*******************************
*********************************************************
data:  begin of lt_LIKP occurs 1, "delivery's table
  VGBEL type VBRP-VGBEL, "delivery's number
    end of lt_LIKP.
SELECT VGBEL INTO lt_LIKP FROM VBRP "put in structure
  WHERE VBELN = lv_VBELN.
  INSERT lt_LIKP INTO TABLE lt_LIKP. "append to table
  ENDSELECT.
data: lv_VLSTK type LIKP-VLSTK.
SELECT SINGLE VLSTK INTO lv_VLSTK FROM LIKP
  WHERE VBELN = lt_LIKP-VGBEL.
    IF lv_VLSTK = 'C'.
       MESSAGE 'Продажа из EWM!!' TYPE 'I'.
    ENDIF.
*********check for transit and MMV***********************
*********************************************************
data:  begin of lt_VBELN_transit occurs 1, "returns table
  VBRP type VBRP-VBELN,
    end of lt_VBELN_transit.
SELECT VBELN INTO lt_VBELN_transit FROM ZWMS_DELIV_CLOSE
  WHERE VALUE = lt_LIKP-VGBEL.
" IF sy-subrc = 0.
"    MESSAGE '"Это ТРАНЗИТ!"' TYPE 'I'.
"  ENDIF.
  INSERT lt_VBELN_transit INTO TABLE lt_VBELN_transit.
  ENDSELECT.
data: lv_TKNUM_tr type ZSTAGE_DRIVER_WF-TKNUM.
LOOP AT lt_VBELN_transit.
SELECT SINGLE TKNUM INTO lv_TKNUM_tr FROM ZSTAGE_DRIVER_WF
  WHERE VBELN = lt_VBELN_transit-VBRP.
    IF lv_TKNUM_tr <> 0.
       MESSAGE 'Это ТРАНЗИТ! и Поставка ММВ, только сторно фактуры, без сторно ОМ!' TYPE 'I'.
    ENDIF.
ENDLOOP.
DATA: lv_VBELN_transit TYPE VBRP-VBELN.
SELECT SINGLE VBELN
  FROM ZWMS_DELIV_CLOSE
    INTO lv_VBELN_transit
      WHERE VALUE = lt_LIKP-VGBEL.
      IF sy-subrc = 0.
        MESSAGE '"Это ТРАНЗИТ!"' TYPE 'I'.
      ENDIF.
*************check for MMV*******************************
*********************************************************
data: lv_TKNUM type ZSTAGE_DRIVER_WF-TKNUM.
lv_TKNUM = 0.
SELECT SINGLE TKNUM INTO lv_TKNUM FROM ZSTAGE_DRIVER_WF
  WHERE VBELN = lt_LIKP-VGBEL.
    IF lv_TKNUM <> 0.
       MESSAGE 'Поставка ММВ, только сторно фактуры, без сторно ОМ!' TYPE 'I'.
    ENDIF.
*************check for ZSTATUS***************************
*********************************************************
DELETE ADJACENT DUPLICATES FROM lt_LIKP.
data: lv_ZSTATUS type ZSTAGE_DRIVER_WF-ZSTATUS.
data: lv_issued(1).
DATA: ZSTATUS_TEXT(120) type C.
DATA(const_tknum) = |{ CONV tknum( 8000000 ) ALPHA = IN }|.
LOOP AT lt_LIKP.
SELECT SINGLE ZSTATUS INTO lv_ZSTATUS FROM ZSTAGE_DRIVER_WF
  WHERE VBELN = lt_LIKP-VGBEL and
        ZSTATUS = '001' and
        TKNUM < const_tknum.
    IF lv_ZSTATUS <> 0.
          CONCATENATE 'ВНИМАНИЕ поставка' lt_LIKP-VGBEL 'ТОВАР ВЫДАН' INTO ZSTATUS_TEXT SEPARATED BY space.
          MESSAGE ZSTATUS_TEXT TYPE 'I'.
          lv_issued = 'Y'.
    ENDIF.
ENDLOOP.
*************get FUTD data*******************************
*********************************************************
DATA:
  begin of ls_DATA_FUTD,
  WERKS type VBRP-WERKS, "plant
  VTWEG_AUFT type VBRP-VTWEG_AUFT, "distribute channel
  SPART type VBRP-SPART, "division
      end of ls_DATA_FUTD.
SELECT SINGLE WERKS VTWEG_AUFT SPART INTO ls_DATA_FUTD FROM VBRP
  WHERE VBELN = lv_VBELN.
DATA ULFUTD(4).
*get data from 202
IF lv_FKART = 'RE'.
MESSAGE 'Это возврат, перед сторно проверь остатки!' TYPE 'I'.
  SELECT SINGLE W_OWNER
    FROM ZFUTN_RETURN
    "CLIENT SPECIFIED - IF request is from another client (MANDT)
    CLIENT SPECIFIED INTO ULFUTD
   WHERE MANDT = '202' and VBELN_VF = lv_VBELN.
ELSE.
SELECT SINGLE W_OWNER3
  FROM ZFUTN_MSALE
  CLIENT SPECIFIED INTO ULFUTD
  WHERE MANDT = '202' and VBELN_VF = lv_VBELN.
ENDIF.
DATA: lv_FUTD_SPART_CHECK(1). "check division
lv_FUTD_SPART_CHECK = 'Y'.
"http://abap4.ru/regular-expression.html, check division consist 2 numbers
FIND regex '\d\d' IN ls_DATA_FUTD-SPART.
  IF sy-subrc = 0.
    lv_FUTD_SPART_CHECK = 'N'.
  ELSE.
    IF ULFUTD IS INITIAL.
      MESSAGE 'ЮЛ ФУТД не определено!' TYPE 'I'.
      ENDIF.
  ENDIF.
*************check plant*********************************
*********************************************************
DATA: lt_werks_nn TYPE RANGE OF bukrs, lv_NNOV(1).
  lt_werks_nn = VALUE #(
    ( sign = 'I' option = 'EQ' low = '4120' )
    ( sign = 'I' option = 'EQ' low = '5120' )
    ( sign = 'I' option = 'EQ' low = 'CE07' )
    ( sign = 'I' option = 'EQ' low = 'HE07' )
    ( sign = 'I' option = 'EQ' low = 'NE07' )
    ( sign = 'I' option = 'EQ' low = 'SE07' )
  ).
IF  ls_DATA_FUTD-WERKS IN lt_werks_nn AND lv_TKNUM = 0."IF not MMV.
       MESSAGE 'Это Нижний, они просят делать только сторно фактуры и ОМ, поставку не обнулять, заказ не отклонять' TYPE 'I'.
       lv_NNOV = 'Y'.
    ENDIF.
*********order's table***********************************
*********************************************************
data:  begin of lt_ORDER occurs 1,
  AUBEL type VBRP-AUBEL, "order num
  AUPOS type VBRP-AUPOS, "opder pos
    end of lt_ORDER.
SELECT AUBEL AUPOS INTO lt_ORDER FROM VBRP
  WHERE VBELN = lv_VBELN.
  INSERT lt_ORDER INTO TABLE lt_ORDER.
  ENDSELECT.
**********check special stock E**************************
*********************************************************
DATA: lv_SOBKZ(1).
DATA: lv_E_ZAPAS(1).
lv_E_ZAPAS(1) = 'N'.
LOOP AT lt_ORDER.
SELECT SINGLE SOBKZ INTO lv_SOBKZ FROM VBAP
  WHERE VBELN = lt_ORDER-AUBEL.
    IF lv_SOBKZ  = 'E'.
      lv_E_ZAPAS = 'Y'.
    ENDIF.
ENDLOOP.
**********text for closing*******************************
*********************************************************
DATA: lv_DATE type VBRP-ERDAT."invoice date
DATA: lv_DAY(2), lv_MON(2), lv_YEAR(4), lv_DATE_OUT(10).
SELECT SINGLE ERDAT INTO lv_DATE FROM VBRP
  WHERE VBELN = lv_VBELN.
  lv_YEAR = lv_DATE(4).
  lv_MON = lv_DATE+4(2).
  lv_DAY = lv_DATE+6(2).
CONCATENATE lv_DAY '.' lv_MON '.' lv_YEAR  INTO lv_DATE_OUT.
DATA: lv_OUT_TEXT1(120) type C, lv_OUT_TEXT3(120) type C, lv_OUT_TEXT4(120) type C.
CONCATENATE 'Фактура' lv_VBELN 'от' lv_DATE_OUT 'сторнирована' INTO lv_OUT_TEXT1 SEPARATED BY space.
IF lv_FUTD_SPART_CHECK = 'N'. "сектор из 2х цифр"
  lv_OUT_TEXT3 = '1С БП без изменений'.
ELSE.
  lv_OUT_TEXT3 = '1С БП проверить наличие и удалить док ERP'.
CONCATENATE '№' lv_VBELN 'от' lv_DATE_OUT ULFUTD INTO lv_OUT_TEXT4 SEPARATED BY space.
ENDIF.
"IF lv_NO_FUTD = 'Y'.
"  exit.
"ELSE.
"CALL FUNCTION 'POPUP_TO_INFORM' "выпадающее окно на 4 строки"
"    EXPORTING
"      titel = 'Для копирования в наряд'
"      txt1  = lv_OUT_TEXT1
"      txt2  = lv_OUT_TEXT2
"      txt3  = lv_OUT_TEXT3
"      txt4  = lv_OUT_TEXT4.
"ENDIF.
DATA :  li_txline  TYPE STANDARD TABLE OF txline, lv_INDEX(1) TYPE I.
lv_INDEX = 1.
INSERT lv_OUT_TEXT1 INTO li_txline INDEX lv_INDEX.
lv_INDEX = lv_INDEX + 1.
IF lv_NNOV = 'Y'.
  INSERT 'ОМ сторнирован.' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
  INSERT 'Поставка НЕ обнулена, заказ НЕ отклонен' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
ELSEIF lv_TKNUM <> 0 and lv_issued <> 'Y'."IF MMV
  INSERT 'Дальнейшие документы необходимо проводить в АРМ Приемки от водителей' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
ELSEIF lv_FKART = 'RE'.
  INSERT 'Кредитовое авизо сторнировано, ПМ сторнирован, возврат отклонен' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
  INSERT '' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
ELSE.
  INSERT 'ОМ сторнирован, поставка обнулена, заказ отклонен' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
ENDIF.
IF lv_VLSTK = 'C' AND lv_TKNUM = 0."IF EWM and no MMV.
  INSERT 'Материал перемещен в скл.место OTKAZ_DOSTAVKA.' INTO li_txline INDEX lv_INDEX.
  lv_INDEX = lv_INDEX + 1.
ELSE.
  IF lv_TKNUM = 0."IF no MMV
    INSERT 'Товар в 916 типе склада.' INTO li_txline INDEX lv_INDEX.
    lv_INDEX = lv_INDEX + 1.
  ENDIF.
ENDIF.
IF lv_E_ZAPAS = 'Y'.
  IF lv_TKNUM = 0."IF no MMV
    INSERT 'ВАЖНО: Проверить Е-запас, при необходимости вывести' INTO li_txline INDEX lv_INDEX.
    lv_INDEX = lv_INDEX + 1.
  ENDIF.
ENDIF.
INSERT '' INTO li_txline INDEX lv_INDEX.
lv_INDEX = lv_INDEX + 1.
INSERT '' INTO li_txline INDEX lv_INDEX.
lv_INDEX = lv_INDEX + 1.
INSERT lv_OUT_TEXT3 INTO li_txline INDEX lv_INDEX.
lv_INDEX = lv_INDEX + 1.
INSERT lv_OUT_TEXT4 INTO li_txline INDEX lv_INDEX.
lv_INDEX = lv_INDEX + 1.
"INSERT '' INTO li_txline INDEX 8.
"INSERT '' INTO li_txline INDEX 9.
"INSERT 'Для сторно нажмите галочку, для продолжения без сторно крестик' INTO li_txline INDEX 9.
CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR' "text window
  EXPORTING
   im_title        = 'Для копирования в наряд'
   im_display_mode = 'X' "or "" to active for input
  CHANGING
   ch_text         = li_txline[].
IF sy-ucomm = 'CX_CONT'."IF press check mark
  "ENDIF.
ELSEIF sy-ucomm = 'CX_CANC'."IF press cross
ENDIF.
*************run_trans_in_a_new_screen********************
**********************************************************
*REPORT  ZMODE.
*CALL FUNCTION 'TH_CREATE_MODE'
*EXPORTING
*   TRANSAKTION          = 'SE38'
**   DEL_ON_EOT           = 0
** IMPORTING
**   MODE                 =
** EXCEPTIONS
**   MAX_SESSIONS         = 1
**   INTERNAL_ERROR       = 2
**   NO_AUTHORITY         = 3
**   OTHERS               = 4
          .
*IF SY-SUBRC <> 0.
*ENDIF.
DATA: F11_PARAMS type STRING.
CONCATENATE 'KOMFK-VBELN=' lv_VBELN INTO F11_PARAMS.
CALL FUNCTION 'TH_CREATE_MODE'
   EXPORTING
     TRANSAKTION = 'VF11'
     parameters = F11_PARAMS.