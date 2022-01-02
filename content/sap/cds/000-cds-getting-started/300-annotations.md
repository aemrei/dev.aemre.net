---
title: Annotations
---

# Annotations

## @ObjectModel.dataCategory

```cds
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'mytype'
```

## @Aggregation.default: #SUM

```cds
    @Aggregation.default: #SUM
    @Semantics.quantity.unitOfMeasure: 'myunit'
    myquantity,
```

## @AbapCatalog.preserveKey:true

```cds
Transfer transparent table keys to view
```

## Text and Compositional Relations

```cds
@ObjectModel.compositionRoot: true
define view Z21_C_HEADER as select from Z21_Header
    association [1..*] to Z21_C_Item as _item
        on $projection.mykey = _item.mykey
    association [1..*] to Z21_C_Type as _typetext
        on $projection.mytype = _typetext.mytype
{
    key mykey,

    //For other foreign keys @ObjectModel.foreign.association: '_typetext'
    @ObjectModel.text.association: '_typetext'
    mytype,

    @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
    _item, // Make association public

    _typetext
}
```

```cds
define view Z21_C_Item as select from Z21_Item
  association [1..1] to z21_c_header as _header
{
  @ObjectModel.association.type: [#TO_COMPOSITION_PARENT, #TO_COMPOSITION_ROOT]
  _header
}
```
