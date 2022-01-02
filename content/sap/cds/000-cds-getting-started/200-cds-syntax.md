---
title: CDS Syntax
---

# CDS Syntax

## Cast Operations

```cds
cast(myunit as abap.unit(3)) as myunit,

key cast(spras as sylangu preserving type) as spras,
```

## Case

```cds
    case when myquantity < 30 then '<' else '>' end as comp,
    case myquantity when 30 then '=' else '<>' end as equ
```



## Session Variables

```cds
define view Z_ViewWithSessionVariables
  as select from t000
{
  $session.client as ClientField, //sy-mandt
  $session.system_date as SystemDateField, //sy-datum
  $session.system_language as SystemLanguageField, //sy-langu
  $session.user as UserField //sy-uname
} where mandt = $session.client
```

## Join

```cds
define view Z_ViewWithLeftOuterJoin
  as select from Z_ViewAsDataSourceD
  left outer to many join Z_ViewAsDataSourceE
    on Z_ViewAsDataSourceD.FieldD2 = Z_ViewAsDataSourceE.FieldE1


define view Z_ViewWithoutAssociationPaths
  as select from ZI_SalesOrderScheduleLine as SL
  left outer to one join ZI_SalesOrderItem as ITEM
    on ITEM.SalesOrder = SL.SalesOrder
```

> You should always specify the maximum target cardinality (TO ONE or TO MANY) of a left outer join partner. On one hand, this specification is used to document the composition of the CDS view. On the other hand, this information can optimize the processing of a selection request in the database.
>
> However, if you maintain the cardinality information, you should make sure that it's defined correctly. Otherwise, the selection result may become incorrect.

## Aggregation Functions

```cds
@AbapCatalog.sqlViewName: 'Z21_V_AGGR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Aggregations'
define view z21_c_aggr as select from Z21_C_Item {
    key mykey,
    avg( myquantity ) as myavg,
    min( myquantity ) as mymin,
    max( myquantity ) as mymax,
    count(*) as mycount
} group by mykey

```

## Association

| Cardinality | Min/Max |
| ----------- | ------- |
| default     | 0-1     |
| [1]         | 0-1     |
| [0..1]      | 0-1     |
| [1..1]      | 1-1     |
| [0..*]      | 0-∞     |
| [1..*]      | 1-∞     |

## Parameters

CDS: `Z21_C_type_param`

```cds
@AbapCatalog.sqlViewName: 'Z21_VP_TYPE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS Type with param'
define view Z21_C_type_param
 with parameters p_lang: sylangu
 as select from Z21_C_Type {
    key mytype,
    mytext
} where spras = $parameters.p_lang
```

SQL View for a parametric view in ABAP code

```abap
SELECT
   Z21_C_TYPE_PARAM~MYTYPE,
   Z21_C_TYPE_PARAM~MYTEXT
 FROM
  Z21_C_TYPE_PARAM( P_LANG = 'T' )
```

## Conversion

```cds
{
unit_conversion( quantity => OrderQuantity,
                 source_unit => OrderQuantityUnit,
                 target_unit => :P_DisplayUnit,
                 error_handling => 'FAIL_ON_ERROR' ) as mynewquantity

}
```

