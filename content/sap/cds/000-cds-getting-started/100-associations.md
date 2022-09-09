---
title: Associations
---

# Associations

## Header to item association

CDS: `Z21_C_HEADER`

```cds
@AbapCatalog.sqlViewName: 'Z21_VV_HEADER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS Header'
define view Z21_C_HEADER as select from Z21_Header
    association [1..*] to Z21_Item as _item
    on $projection.mykey = _item.mykey
{
    key mykey,
    _item // Make association public
}
```

## Header to text association

CDS: `Z21_C_HEADER`

```cds
@AbapCatalog.sqlViewName: 'Z21_VV_HEADER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS Header'
define view Z21_C_HEADER as select from Z21_Header
    association [1..*] to Z21_Item as _item
        on $projection.mykey = _item.mykey
    association [1..*] to Z21_Type as _typetext
        on $projection.mytype = _typetext.mytype
{
    key mykey,
    mytype,
    _item, // Make association public
    _typetext
}
```

## SQL Console

View and follow associations with **F8**, then you can show SQL command from **SQL Console** button.

```cds
SELECT 
   \_ITEM-MYKEY AS MYKEY , 
   \_ITEM-MYSUBKEY AS MYSUBKEY , 
   \_ITEM-MYSUBVALUE AS MYSUBVALUE , 
   \_ITEM-MYQUANTITY AS MYQUANTITY , 
   \_ITEM-MYUNIT AS MYUNIT
 FROM 
  Z21_C_HEADER
 WHERE 
  Z21_C_HEADER~MYKEY = 'id01'
 AND \_ITEM-MYKEY IS NOT NULL
```

## SQL Dependency Tree

Right click ➡️Open with ➡️Dependency Tree

You can also see **Dependency Graph** in this window.

## Active Annotations

Right click ➡️Open with ➡️Active Annotations

## CDS Navigator

Right to Project Explorer, click the CDS Navigator. You can see CDS Extensions, metadata extensions etc.
