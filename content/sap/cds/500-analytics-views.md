---
title: Analytics Views
---

# Analytics Views

> To test analytic views, go to transaction: `RSRTS_ODP_DIS`



```cds
@AbapCatalog.sqlViewName: 'ZB_SOIC01'
@EndUserText.label: 'Analytic Cube 01 for Sales Order Items'
@Analytics.dataCategory: #CUBE
define view ZB_SalesOrderItemCube01
  as select from I_SalesOrderItem
{
  SalesOrder,
  _SalesOrder,
  _Material, 
  
  @Aggregation.default: #SUM
  OrderQuantity,
  OrderQuantityUnit,
  _OrderQuantityUnit, 
  @Aggregation.default: #SUM
  NetAmount,
  TransactionCurrency,
  _TransactionCurrency
}
```



```cds
@AbapCatalog.sqlViewName: 'ZB_SOIQ01'
@EndUserText.label: 'Query 01 for sales order items'
@Analytics.query: true
define view ZB_SalesOrderItemQuery01
  as select from ZB_SalesOrderItemCube01
{
  Material,
  SoldToParty,
  SoldToCountry,
  OrderQuantity,
  NetAmount
}
```


