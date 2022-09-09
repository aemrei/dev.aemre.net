---
title: ZCL_EMAIL
metaTitle: ABAP email utility class and its usage
metaDescription: ZCL_EMAIL class for sending mail from ABAP code with nice formatted body.
---

## Usage:
```abap
*&---------------------------------------------------------------------*
*& Report ZEI_MAIL
*&---------------------------------------------------------------------*
REPORT ZEI_MAIL.

START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
FORM main.
  DATA(lr_mail) = NEW zcl_email( iv_recipient = 'SAPUSER;deneme@example.com' iv_subject = 'Mail Subject' ).

  lr_mail->add_body( 'Basit içerik' )->add_body( iv_body = 'Başlıklı başka bişey' iv_title = 'Başlık' )->add_body( 'Bambaşka bişey ' ).

  lr_mail->add_body( iv_title = 'Uzun Bir Metnin Başlığı'
                     iv_body = `Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do` &&
                               ` eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut` &&
                               ` enim ad minim veniam, quis nostrud exercitation ullamco labori` &&
                               `s nisi ut aliquip ex ea commodo consequat. Duis aute irure dolo` &&
                               `r in reprehenderit in voluptate velit esse cillum dolore eu fug` &&
                               `iat nulla pariatur. Excepteur sint occaecat cupidatat non proid` &&
                               `ent, sunt in culpa qui officia deserunt mollit anim id est labo` &&
                               `rum.` ).

  SELECT *
    INTO TABLE @DATA(lt_spfli)
    FROM spfli.

  lr_mail->add_body( iv_title = 'Uçak verileri'
                     iv_body = 'Aşağıdaki gibi bir veri oluşmuştur'
                     iv_data = lt_spfli ).

  lr_mail->add_attachment( iv_name = 'EkOlarakTablo.csv'
                           iv_type = 'CSV'
                           iv_xstring = zcl_utils=>table_to_excel( lt_spfli ) ).


  lr_mail->add_body( iv_body = '<p>Bilgi için <a href="https://www.google.com/search?q=bilgi">buraya</a> tıklayabilirsiniz.</p>' ).

  "İstenirse aşağıdaki gibi sender adresi konabilir"
  lr_mail->set_sender( 'SAPJOB' ).


*  lr_mail->send( iv_preview_only = abap_true ). "Maili göndermeden bakar
  lr_mail->send( iv_preview_only = abap_true ).
ENDFORM.


*&---------------------------------------------------------------------*
*& Form SIMPLE_MAIL
*&---------------------------------------------------------------------*
FORM simple_mail.
  NEW zcl_email( iv_subject = 'Konu'
                 iv_body = 'İçerik'
                 iv_recipient = 'SAPUSER; deneme@deneme.com' )->send( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SIMPLE_MAIL2
*&---------------------------------------------------------------------*
FORM simple_mail2.
  DATA lr_mail TYPE REF TO zcl_email.

  lr_mail = NEW #( iv_subject = 'Link içeren mail' iv_recipient = CONV #( sy-uname ) ).
  lr_mail->add_body( 'Buraya <a href="https://www.google.com/search?q=mail+gonderme">tıklayın</a>').
  lr_mail->send( ).
ENDFORM.


*&---------------------------------------------------------------------*
*& Form UZUN_MAIL
*&---------------------------------------------------------------------*
FORM uzun_mail.
  DATA(lr_mail) = NEW zcl_email( iv_recipient = 'SAPUSER;deneme@example.com' iv_subject = 'Mail Subject' ).

  lr_mail->add_body( 'Basit içerik' )->add_body( iv_body = 'Başlıklı başka bişey' iv_title = 'Başlık' )->add_body( 'Bambaşka bişey ' ).

  SELECT *
    INTO TABLE @DATA(lt_spfli)
    FROM spfli.

  lr_mail->add_body( iv_title = 'Uçak verileri'
                     iv_body = 'Aşağıdaki gibi bir veri oluşmuştur'
                     iv_data = lt_spfli ).


  lr_mail->add_body( iv_title = 'Uzun Bir Metnin Başlığı'
                     iv_body = `Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do` &&
                               ` eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut` &&
                               ` enim ad minim veniam, quis nostrud exercitation ullamco labori` &&
                               `s nisi ut aliquip ex ea commodo consequat. Duis aute irure dolo` &&
                               `r in reprehenderit in voluptate velit esse cillum dolore eu fug` &&
                               `iat nulla pariatur. Excepteur sint occaecat cupidatat non proid` &&
                               `ent, sunt in culpa qui officia deserunt mollit anim id est labo` &&
                               `rum.` ).
  "İstenirse aşağıdaki gibi sender adresi konabilir"
  lr_mail->set_sender( 'SAPJOB' ).


*  lr_mail->send( iv_preview_only = abap_true ). "Maili göndermeden önizlemesine bak"
  lr_mail->send( ).
ENDFORM.


*&---------------------------------------------------------------------*
*& Form EKLI_UZUN_MAIL
*&---------------------------------------------------------------------*
FORM ekli_uzun_mail.
  DATA(lr_mail) = NEW zcl_email( iv_recipient = 'SAPUSER;deneme@example.com' iv_subject = 'Mail Subject' ).

  lr_mail->add_body( 'Basit içerik' )->add_body( iv_body = 'Başlıklı başka bişey' iv_title = 'Başlık' )->add_body( 'Bambaşka bişey ' ).

  lr_mail->add_body( iv_title = 'Uzun Bir Metnin Başlığı'
                     iv_body = `Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do` &&
                               ` eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut` &&
                               ` enim ad minim veniam, quis nostrud exercitation ullamco labori` &&
                               `s nisi ut aliquip ex ea commodo consequat. Duis aute irure dolo` &&
                               `r in reprehenderit in voluptate velit esse cillum dolore eu fug` &&
                               `iat nulla pariatur. Excepteur sint occaecat cupidatat non proid` &&
                               `ent, sunt in culpa qui officia deserunt mollit anim id est labo` &&
                               `rum.` ).

  SELECT *
    INTO TABLE @DATA(lt_spfli)
    FROM spfli.

  lr_mail->add_body( iv_title = 'Uçak verileri'
                     iv_body = 'Aşağıdaki gibi bir veri oluşmuştur'
                     iv_data = lt_spfli ).

  lr_mail->add_attachment( iv_name = 'EkOlarakTablo.csv'
                           iv_type = 'CSV'
                           iv_xstring = zcl_utils=>table_to_excel( lt_spfli ) ).


  lr_mail->add_body( iv_body = '<p>Bilgi için <a href="https://www.google.com/search?q=bilgi">buraya</a> tıklayabilirsiniz.</p>' ).

  "İstenirse aşağıdaki gibi sender adresi konabilir"
  lr_mail->set_sender( 'SAPJOB' )->send( ).
ENDFORM.
```


