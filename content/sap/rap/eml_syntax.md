---
title: EML Syntax
---

## Read

```abap
    READ ENTITIES OF z21_i_header
         ENTITY header
          ALL FIELDS WITH CORRESPONDING #( entities )
           RESULT DATA(lt_headers).
```

## Create

```abap
MODIFY ENTITIES OF ZDH_MngdDocumentHeader
      ENTITY Header
        CREATE SET FIELDS WITH VALUE #( ( %cid                   = 'doc1'
                                          CompanyCode            = '0001'
                                          PurchasingOrganization = '0001' ) )
```

```abap
MODIFY ENTITIES OF z21_i_header IN LOCAL MODE
  ENTITY header
    UPDATE FIELDS ( Netvalue ) WITH CORRESPONDING #( lt_service_orders )
    CREATE BY \_BillingPlan
      SET FIELDS WITH billing_cba
```

## Augmentation

```abap
DATA lt_update_cba TYPE TABLE FOR UPDATE z21_i_item.

APPEND INITIAL LINE TO lt_update_cba ASSIGNING FIELD-SYMBOL(<ls_item_cba>).
<ls_item_cba> = VALUE #(
    BASE CORRESPONDING #( <ls_entity>-%target[ 1 ] )
    %cid_ref = <ls_entity>-%target[ 1 ]-%cid
    %pid = <ls_entity>-%pid

    Field1 = 'ABC'
    Field2 = '1'
    %control = VALUE #( BASE CORRESPONDING #( <ls_entity>-%target[ 1 ]-%control )
                  Field1 = if_abap_behv=>mk-on
                  Field2 = if_abap_behv=>mk-on
               )
).

MODIFY AUGMENTING ENTITIES OF z21_i_header ENTITY item UPDATE FROM lt_update_cba RELATING TO entities BY myrelates.
```
