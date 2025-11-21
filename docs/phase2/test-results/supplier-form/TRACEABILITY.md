# Traceability Report

**Form**: frmsupplier
**Source**: frmsupplier.frm
**Generated**: 4 files

---

## VB6 Source → IR → Angular Code

### UI Controls

#### Combo1
- **VB6 Type**: ComboBox
- **Caption**: ""
- **VB6 Source**: Lines Lines 1-5
- **IR Path**: `ui.controls[].id == 'Combo1'`
- **Angular**: Search for `combo1` in template

#### Command2
- **VB6 Type**: CommandButton
- **Caption**: "Add"
- **VB6 Source**: Lines Lines 24-29
- **IR Path**: `ui.controls[].id == 'Command2'`
- **Angular**: Search for `command2` in template

#### Command3
- **VB6 Type**: CommandButton
- **Caption**: "Delete"
- **VB6 Source**: Lines Lines 18-23
- **IR Path**: `ui.controls[].id == 'Command3'`
- **Angular**: Search for `command3` in template

#### Command4
- **VB6 Type**: CommandButton
- **Caption**: "Edit"
- **VB6 Source**: Lines Lines 12-17
- **IR Path**: `ui.controls[].id == 'Command4'`
- **Angular**: Search for `command4` in template

#### Command5
- **VB6 Type**: CommandButton
- **Caption**: "Reset"
- **VB6 Source**: Lines Lines 6-11
- **IR Path**: `ui.controls[].id == 'Command5'`
- **Angular**: Search for `command5` in template

#### DataGrid1
- **VB6 Type**: DataGrid
- **Caption**: ""
- **VB6 Source**: Lines Lines 54-95
- **IR Path**: `ui.controls[].id == 'DataGrid1'`
- **Angular**: Search for `datagrid1` in template

#### Label1
- **VB6 Type**: Label
- **Caption**: "SUPPLIER MANAGEMENT"
- **VB6 Source**: Lines Lines 96-101
- **IR Path**: `ui.controls[].id == 'Label1'`
- **Angular**: Search for `label1` in template

#### Label2
- **VB6 Type**: Label
- **Caption**: "Supplier Name:"
- **VB6 Source**: Lines Lines 116-121
- **IR Path**: `ui.controls[].id == 'Label2'`
- **Angular**: Search for `label2` in template

#### Label3
- **VB6 Type**: Label
- **Caption**: "Location:"
- **VB6 Source**: Lines Lines 122-127
- **IR Path**: `ui.controls[].id == 'Label3'`
- **Angular**: Search for `label3` in template

#### Label4
- **VB6 Type**: Label
- **Caption**: "Mobile No:"
- **VB6 Source**: Lines Lines 128-133
- **IR Path**: `ui.controls[].id == 'Label4'`
- **Angular**: Search for `label4` in template

#### Label5
- **VB6 Type**: Label
- **Caption**: "Email ID:"
- **VB6 Source**: Lines Lines 134-139
- **IR Path**: `ui.controls[].id == 'Label5'`
- **Angular**: Search for `label5` in template

#### Label6
- **VB6 Type**: Label
- **Caption**: "Search by Name:"
- **VB6 Source**: Lines Lines 140-145
- **IR Path**: `ui.controls[].id == 'Label6'`
- **Angular**: Search for `label6` in template

#### Text1
- **VB6 Type**: TextBox
- **Caption**: ""
- **VB6 Source**: Lines Lines 48-53
- **IR Path**: `ui.controls[].id == 'Text1'`
- **Angular**: Search for `text1` in template

#### Text2
- **VB6 Type**: TextBox
- **Caption**: ""
- **VB6 Source**: Lines Lines 42-47
- **IR Path**: `ui.controls[].id == 'Text2'`
- **Angular**: Search for `text2` in template

#### Text3
- **VB6 Type**: TextBox
- **Caption**: ""
- **VB6 Source**: Lines Lines 36-41
- **IR Path**: `ui.controls[].id == 'Text3'`
- **Angular**: Search for `text3` in template

#### Text4
- **VB6 Type**: TextBox
- **Caption**: ""
- **VB6 Source**: Lines Lines 30-35
- **IR Path**: `ui.controls[].id == 'Text4'`
- **Angular**: Search for `text4` in template

### Event Handlers

#### Command2_Click()
- **Control**: Command2
- **Event**: Click
- **VB6 Source**: Lines Lines 95-113
- **IR Path**: `logic.event_handlers[].handler_name == 'Command2_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 4 steps
  1. Create new recordset for suppliers table
  2. Check all required fields are filled
  3. Add new record to database
  4. Show success confirmation

#### Command3_Click()
- **Control**: Command3
- **Event**: Click
- **VB6 Source**: Lines Lines 115-127
- **IR Path**: `logic.event_handlers[].handler_name == 'Command3_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 2 steps
  1. Show confirmation dialog
  2. Execute delete SQL command

#### Command4_Click()
- **Control**: Command4
- **Event**: Click
- **VB6 Source**: Lines Lines 129-145
- **IR Path**: `logic.event_handlers[].handler_name == 'Command4_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 2 steps
  1. Open recordset for selected supplier
  2. Update supplier fields

#### Command5_Click()
- **Control**: Command5
- **Event**: Click
- **VB6 Source**: Lines Lines 147-150
- **IR Path**: `logic.event_handlers[].handler_name == 'Command5_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 1 steps
  1. Reset form fields to empty

#### Combo1_Click()
- **Control**: Combo1
- **Event**: Click
- **VB6 Source**: Lines Lines 152-157
- **IR Path**: `logic.event_handlers[].handler_name == 'Combo1_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 1 steps
  1. Filter suppliers by name using LIKE query

#### DataGrid1_Click()
- **Control**: DataGrid1
- **Event**: Click
- **VB6 Source**: Lines Lines 159-165
- **IR Path**: `logic.event_handlers[].handler_name == 'DataGrid1_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 1 steps
  1. Fill text boxes with selected row data

#### Form_Load()
- **Control**: frmsupplier
- **Event**: Load
- **VB6 Source**: Lines Lines 167-170
- **IR Path**: `logic.event_handlers[].handler_name == 'Form_Load'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 2 steps
  1. Load supplier data into grid
  2. Populate search combo box

### Validations

#### Text1, Text2, Text3, Text4 Validation
- **Rule**: required
- **Error Message**: "All fields are required!"
- **VB6 Source**: Lines Lines 100-103
- **Angular**: Validator in .component.ts

#### delete_confirmation Validation
- **Rule**: custom
- **Error Message**: "Operation cancelled by user"
- **VB6 Source**: Lines Lines 120-121
- **Angular**: Validator in .component.ts

### Data Operations

#### SELECT suppliers
- **Method**: Command2_Click()
- **VB6 Source**: Lines Lines 2-3
- **Angular**: Service method (if service generated)

#### INSERT suppliers
- **Method**: Command2_Click()
- **VB6 Source**: Lines Lines 9-13
- **Angular**: Service method (if service generated)

#### DELETE suppliers
- **Method**: Command3_Click()
- **VB6 Source**: Lines Lines 7-3
- **Angular**: Service method (if service generated)

#### SELECT suppliers
- **Method**: Command4_Click()
- **VB6 Source**: Lines Lines 2-4
- **Angular**: Service method (if service generated)

#### UPDATE suppliers
- **Method**: Command4_Click()
- **VB6 Source**: Lines Lines 4-8
- **Angular**: Service method (if service generated)

#### SELECT suppliers
- **Method**: Combo1_Click()
- **VB6 Source**: Lines Lines 3-1
- **Angular**: Service method (if service generated)

#### SELECT suppliers
- **Method**: loadgrid()
- **VB6 Source**: Lines Line 3 in loadgrid
- **Angular**: Service method (if service generated)

#### SELECT suppliers
- **Method**: fillcombo()
- **VB6 Source**: Lines Line 3 in fillcombo
- **Angular**: Service method (if service generated)

### Generation Notes

- **Complexity**: 4
- **Estimated Manual Effort**: 1.7 hours
- **Automation Rate**: 85%

**Notes**:
- Create interfaces/services for: ADODB.Recordset
- Fix 2 security issue(s)
- Use Angular Material Dialog for modal behavior
- Use Reactive Forms with validation