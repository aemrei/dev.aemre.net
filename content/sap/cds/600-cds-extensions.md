---
title: CDS Extensions
---

# CDS Extensions

```cds
@AbapCatalog.sqlViewName: 'ESDSLSDOCITMBSC'
@EndUserText.label: 'Extension view for VBAP'
@VDM.viewType: #EXTENSION
define view E_SalesDocumentItemBasic
  as select from vbap as Persistence
{
  key Persistence.vbeln as SalesDocument,
  key Persistence.posnr as SalesDocumentItem
}
```

```cds
@AbapCatalog.sqlViewAppendName: 'ZXESDIBPRIO'
extend view E_SalesDocumentItemBasic
  with ZX_E_SalesDocItemBasic_Prio
  association [0..1] to Z_Priority as _ZZPriority
  on $projection.ZZPriority = _ZZPriority.ZZPriority
{
  @ObjectModel.foreignKey.association: '_ZZPriority'
  Persistence.zzprio as ZZPriority,

  _ZZPriority 
}
```
