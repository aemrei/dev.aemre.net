---
title: Virtual Data Model (VDM)
---

# Virtual Data Model (VDM)

## Naming Convention

| View Type                 |       | @VDM.viewType                                    |
| ------------------------- | ----- | ------------------------------------------------ |
| Basic Interface           | I_*   | #BASIC                                           |
| Composite Interface       | I_*   | #COMPOSITE                                       |
| Transactional Object      | I_*TP | #TRANSACTIONAL                                   |
| Consumption               | C_    | #CONSUMPTION                                     |
| Transactional Consumption | C_*TP | #CONSUMPTION                                     |
| Remote API                | A_    | @VDM.lifecycle.contract.type: #PUBLIC_REMOTE_API |
| Private VDM               | P_    | @VDM.private: true                               |
| Extension Include         | E_    | #EXTENSION                                       |
| VDM View Extension        | X_    | @VDM.viewExtension: true                         |

> `Remote API view`s are used to define OData or web service APIs that enable remote access from other systems to their corresponding VDM interface views.

| Name of Views         | Description              | Example                     |
| --------------------- | ------------------------ | --------------------------- |
| `<ViewName>Text`      | language-dependent texts | I_TaxCodeText               |
| `<ViewName>Cube`      | analytic cube view       | I_SalesOrderItemCube        |
| `<ViewName>Query`     | analytic query           | C_GoodsMovementQuery        |
| `<ViewName>ValueHelp` | value help               | C_CustomerMaterialValueHelp |
| `<ViewName>ObjPg`     | overview page            | C_InboundDeliveryObjPg      |
