---
title: Annotations
metaTitle: Semantic Annotations in SAP CDS
---

# Semantic Annotations

## Field Labels

Change label of a field

```cds
@EndUserText.label: 'Custom label'
@EndUserText.quickInfo: 'Quick Info'
```

For ABAP datatypes, CDS select a longest text whose length is maximum 20 characters.

## Units

```cds
@Semantics.unitOfMeasure: true
OrderQuantityUnit,

@Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
OrderQuantity,



@Semantics.amount.currencyCode: 'TransactionCurrency'
NetAmount,

@Semantics.currencyCode: true
TransactionCurrency,
```

## Default Aggregation

```cds
@DefaultAggregation: #SUM
OrderQuantity,
@DefaultAggregation: #SUM
NetAmount,
@DefaultAggregation: #NONE
NetPriceAmount,
```

## Text Field

```cds
@Semantics.language
key spras as Language,
@Semantics.text: true
cast(landx50 as fis_landx50 preserving type ) as CountryName,
```

## Foreign Keys

```cds
define view I_ProfitCenter as select distinct from cepc
  association[0..1] to I_Country as _Country
    on $projection.Country = _Country.Country
  association[0..1] to I_Region as _Region
    on $projection.Country = _Region.Country and
       $projection.Region = _Region.Region
{
  ...
  @ObjectModel.foreignKey.association: '_Country'
  land1 as Country,

  @ObjectModel.foreignKey.association: '_Region'
  regio as Region,
  ...
}
```

Mark representative keys in target views:

```cds
@ObjectModel.representativeKey: 'Region'

define view I_Region as select from t005s
  association [1..1] to I_Country as _Country
    on $projection.Country = _Country.Country
{
  @ObjectModel.foreignKey.association: '_Country' key t005s.land1 as Country,
  key t005s.bland as Region,
  ...
}
```

## Foreign Text Keys

Mark text tables with `@ObjectModel.dataCategory: #TEXT`

```cds
define view I_Country as select from t005
  association [0..*] to I_CountryText as _Text
    on $projection.Country = _Text.Country
{
  @ObjectModel.text.association: '_Text'
  key cast(land1 as land1_gp preserving type ) as Country,
  ...
}

@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'Country'
define view I_CountryText as select from t005t
{
  key land1 as Country,

  @Semantics.language: true
  key spras as Language,

  @Semantics.text: true
  cast(landx50 as fis_landx50 preserving type ) as CountryName,
}
```

## Composition Relations

Mark with `@ObjectModel.association.type` alternative values: `#TO_COMPOSITION_PARENT, #TO_COMPOSITION_CHILD, #TO_COMPOSITION_ROOT`

```cds
define view I_SalesOrderItem ...
  association [1..1] to I_SalesOrder as _SalesOrder
    on $projection.SalesOrder = _SalesOrder.SalesOrder
  association [0..*] to I_SalesOrderScheduleLine as _ScheduleLine
    on $projection.SalesOrder = _ScheduleLine.SalesOrder
     and $projection.SalesOrderItem = _ScheduleLine.SalesOrderItem
{
  ...
  @ObjectModel.association.type: [#TO_COMPOSITION_PARENT, #TO_COMPOSITION_ROOT]
  _SalesOrder,

  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
  _ScheduleLine,
  ...
}
```
