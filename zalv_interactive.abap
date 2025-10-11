*&---------------------------------------------------------------------*
*& Report  ZALV_INTERACTIVE
*&---------------------------------------------------------------------*
*& Interactive ALV: Double-click a sales order to see its items
*&---------------------------------------------------------------------*
REPORT zalv_interactive.

TABLES: vbak, vbap.

*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,   " Sales Document
         erdat TYPE vbak-erdat,   " Created On
         netwr TYPE vbak-netwr,   " Net Value
       END OF ty_vbak.

TYPES: BEGIN OF ty_vbap,
         vbeln  TYPE vbap-vbeln,  " Sales Document
         posnr  TYPE vbap-posnr,  " Item Number
         matnr  TYPE vbap-matnr,  " Material
         arktx  TYPE vbap-arktx,  " Description
         kwmeng TYPE vbap-kwmeng, " Order Quantity
       END OF ty_vbap.

*--------------------------------------------------------------------*
* DATA DECLARATIONS
*--------------------------------------------------------------------*
DATA: it_vbak TYPE STANDARD TABLE OF ty_vbak,
      wa_vbak TYPE ty_vbak,
      it_vbap TYPE STANDARD TABLE OF ty_vbap,
      wa_vbap TYPE ty_vbap.

DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gv_repid    TYPE sy-repid,
      gv_vbeln    TYPE vbak-vbeln. " To store clicked VBELN

*--------------------------------------------------------------------*
* SELECTION SCREEN
*--------------------------------------------------------------------*
SELECT-OPTIONS s_vbeln FOR vbak-vbeln OBLIGATORY DEFAULT '0000000100' TO '0000000200'.

*--------------------------------------------------------------------*
* START-OF-SELECTION
*--------------------------------------------------------------------*
START-OF-SELECTION.

  " Fetch Header Data
  SELECT vbeln erdat netwr
    FROM vbak
    INTO TABLE it_vbak
    WHERE vbeln IN s_vbeln.

  IF sy-subrc <> 0.
    MESSAGE 'No sales orders found!' TYPE 'I'.
    EXIT.
  ENDIF.

  PERFORM build_fieldcat_vbak.

*--------------------------------------------------------------------*
* Display Header ALV
*--------------------------------------------------------------------*
  gv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = gv_repid
      i_callback_user_command = 'USER_COMMAND'  " handle double-click
      is_layout               = gs_layout
      it_fieldcat             = it_fieldcat
    TABLES
      t_outtab                = it_vbak
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

*--------------------------------------------------------------------*
* FORM ROUTINES
*--------------------------------------------------------------------*
FORM build_fieldcat_vbak.
  CLEAR it_fieldcat.

  wa_fieldcat-fieldname = 'VBELN'.
  wa_fieldcat-seltext_m = 'Sales Document'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'ERDAT'.
  wa_fieldcat-seltext_m = 'Created On'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'NETWR'.
  wa_fieldcat-seltext_m = 'Net Value'.
  APPEND wa_fieldcat TO it_fieldcat.

  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
ENDFORM.

*--------------------------------------------------------------------*
* USER COMMAND (handles double-click)
*--------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.  " Double-click event
      READ TABLE it_vbak INTO wa_vbak INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        gv_vbeln = wa_vbak-vbeln.
        PERFORM display_vbap.  " show item details
      ENDIF.
  ENDCASE.

ENDFORM.

*--------------------------------------------------------------------*
* Display VBAP for selected VBELN
*--------------------------------------------------------------------*
FORM display_vbap.

  CLEAR it_vbap.

  SELECT vbeln posnr matnr arktx kwmeng
    FROM vbap
    INTO TABLE it_vbap
    WHERE vbeln = gv_vbeln.

  IF sy-subrc <> 0.
    MESSAGE |No items found for order { gv_vbeln }| TYPE 'I'.
    RETURN.
  ENDIF.

  REFRESH it_fieldcat.
  PERFORM build_fieldcat_vbap.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = it_vbap
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.

*--------------------------------------------------------------------*
* Build Field Catalog for VBAP
*--------------------------------------------------------------------*
FORM build_fieldcat_vbap.
  CLEAR it_fieldcat.

  wa_fieldcat-fieldname = 'POSNR'.
  wa_fieldcat-seltext_m = 'Item No'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-seltext_m = 'Material'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'ARKTX'.
  wa_fieldcat-seltext_m = 'Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'KWMENG'.
  wa_fieldcat-seltext_m = 'Quantity'.
  APPEND wa_fieldcat TO it_fieldcat.

  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
ENDFORM.
