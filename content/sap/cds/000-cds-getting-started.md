---
title: Getting Started - CDS
---

# Getting Started - CDS

## Create Basic View

Create a fake DB line using CDS

```cds
define view Z21_Header as select from t000
{
    key 'id01' as mykey,
    't1' as mytype,
    'value01' as myvalue
} where mandt = $session.client
```



## Sample header and item table views

CDS: `Z21_Header`

```cds
@AbapCatalog.sqlViewName: 'Z21_V_HEADER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Header'
define view Z21_Header as select from t000
{
    key 'id01' as mykey,
    'o' as mytype,
    'value01' as myvalue
} where mandt = $session.client

union all
select from t000 
{
    key 'id02' as mykey,
    'e' as mytype,
    'value02' as myvalue
} where mandt = $session.client

union all
select from t000 
{
    key 'id03' as mykey,
    'o' as mytype,
    'value03' as myvalue
} where mandt = $session.client

union all
select from t000 
{
    key 'id04' as mykey,
    'e' as mytype,
    'value04' as myvalue
} where mandt = $session.client
```

CDS: `Z21_Type`

```cds
@AbapCatalog.sqlViewName: 'Z21_V_ITEM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Item table'
define view Z21_Type as select from t000
{
    key 'id01' as mykey,
    key 'subid01' as mysubkey,
    'subvalue01' as mysubvalue,
    10 as myquantity,
    'ST' as myunit
} where mandt = $session.client

union all
select from t000 
{
    key 'id01' as mykey,
    key 'subid02' as mysubkey,
    'subvalue02' as mysubvalue,
    20 as myquantity,
    'ST' as myunit
} where mandt = $session.client

union all
select from t000 
{
    key 'id01' as mykey,
    key 'subid03' as mysubkey,
    'subvalue03' as mysubvalue,
    30 as myquantity,
    'ST' as myunit
} where mandt = $session.client

union all
select from t000 
{
    key 'id02' as mykey,
    key 'subid01' as mysubkey,
    'subvalue01' as mysubvalue,
    40 as myquantity,
    'ST' as myunit
} where mandt = $session.client

```

CDS: `Z21_Type`

```cds
@AbapCatalog.sqlViewName: 'Z21_V_TYPE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Type Table'
define view Z21_Type as select from t000
{
    key 'o' as mytype,
    key cast('E' as abap.lang) as spras,
    cast( 'odd' as abap.char( 4 ) ) as mytext
} where mandt = $session.client

union all
select from t000 
{
    key 'e' as mytype,
    key 'E' as spras,
    'even' as mytext
} where mandt = $session.client

union all
select from t000 
{
    key 'o' as mytype,
    key 'T' as spras,
    'tek' as mytext
} where mandt = $session.client

union all
select from t000 
{
    key 'e' as mytype,
    key 'T' as spras,
    'cift' as mytext
} where mandt = $session.client

```


