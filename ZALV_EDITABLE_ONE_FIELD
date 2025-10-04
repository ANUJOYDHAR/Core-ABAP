REPORT zalv_editbale_demo.
*--------------------------------------------------------------------*
* Data Declarations
*--------------------------------------------------------------------*
TABLES: vbak.

TYPES: BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,   " Sales Document
         vkorg TYPE vbak-vkorg,   " Sales Organization
         vtweg TYPE vbak-vtweg,   " Distribution Channel
         spart TYPE vbak-spart,   " Division
         erdat TYPE vbak-erdat,   " Created On
         ernam TYPE vbak-ernam,   " Created By
         auart TYPE vbak-auart,   " Document Type (Editable)
       END OF ty_vbak.

DATA: lt_vbak     TYPE TABLE OF ty_vbak,
      ls_vbak     TYPE ty_vbak,
      lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_fieldcat TYPE slis_fieldcat_alv.

*--------------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------------*
SELECT-OPTIONS: so_vbeln FOR vbak-vbeln.

*--------------------------------------------------------------------*
* START-OF-SELECTION
*--------------------------------------------------------------------*
START-OF-SELECTION.

  " Fetch data from VBAK filtered by select-options
  SELECT vbeln vkorg vtweg spart erdat ernam auart
    FROM vbak
    INTO TABLE lt_vbak
    WHERE vbeln IN so_vbeln.

  IF sy-subrc <> 0.
    MESSAGE 'No sales orders found!' TYPE 'I'.
    EXIT.
  ENDIF.

  PERFORM build_fieldcat.
  PERFORM display_alv.

*--------------------------------------------------------------------*
* Build Field Catalog
*--------------------------------------------------------------------*
FORM build_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VBELN'.
  ls_fieldcat-seltext_m = 'Sales Doc'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKORG'.
  ls_fieldcat-seltext_m = 'Sales Org'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VTWEG'.
  ls_fieldcat-seltext_m = 'Dist. Channel'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SPART'.
  ls_fieldcat-seltext_m = 'Division'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ERDAT'.
  ls_fieldcat-seltext_m = 'Created On'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ERNAM'.
  ls_fieldcat-seltext_m = 'Created By'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AUART'.
  ls_fieldcat-seltext_m = 'Doc Type'.
  ls_fieldcat-edit      = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

ENDFORM.

*--------------------------------------------------------------------*
* Display ALV
*--------------------------------------------------------------------*
FORM display_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = lt_fieldcat

    TABLES
      t_outtab      = lt_vbak
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error displaying ALV' TYPE 'E'.
  ENDIF.

ENDFORM.
