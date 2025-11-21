# Traceability Report

**Form**: StartForm
**Source**: StartForm.frm
**Generated**: 4 files

---

## VB6 Source → IR → Angular Code

### UI Controls

#### cmdClose
- **VB6 Type**: CommandButton
- **Caption**: "Close"
- **VB6 Source**: Lines Lines 14-20
- **IR Path**: `ui.controls[].id == 'cmdClose'`
- **Angular**: Search for `cmdclose` in template

#### cmdNew
- **VB6 Type**: CommandButton
- **Caption**: "New"
- **VB6 Source**: Lines Lines 28-34
- **IR Path**: `ui.controls[].id == 'cmdNew'`
- **Angular**: Search for `cmdnew` in template

#### cmdOpen
- **VB6 Type**: CommandButton
- **Caption**: "Open"
- **VB6 Source**: Lines Lines 21-27
- **IR Path**: `ui.controls[].id == 'cmdOpen'`
- **Angular**: Search for `cmdopen` in template

#### Label1
- **VB6 Type**: Label
- **Caption**: "Client ID"
- **VB6 Source**: Lines Lines 42-58
- **IR Path**: `ui.controls[].id == 'Label1'`
- **Angular**: Search for `label1` in template

#### txtID
- **VB6 Type**: TextBox
- **Caption**: ""
- **VB6 Source**: Lines Lines 35-41
- **IR Path**: `ui.controls[].id == 'txtID'`
- **Angular**: Search for `txtid` in template

### Event Handlers

#### cmdNew_Click()
- **Control**: cmdNew
- **Event**: Click
- **VB6 Source**: Lines Lines 71-78
- **IR Path**: `logic.event_handlers[].handler_name == 'cmdNew_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 4 steps
  1. Create new Client object
  2. Create new ClientEdit form
  3. Pass client to form
  4. Show form as modal dialog

#### cmdOpen_Click()
- **Control**: cmdOpen
- **Event**: Click
- **VB6 Source**: Lines Lines 80-95
- **IR Path**: `logic.event_handlers[].handler_name == 'cmdOpen_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 5 steps
  1. Create ClientEdit form
  2. Get client by ID from database
  3. Check if client was found
  4. Show error message if client not found
  5. Show form with existing client

#### cmdClose_Click()
- **Control**: cmdClose
- **Event**: Click
- **VB6 Source**: Lines Lines 97-98
- **IR Path**: `logic.event_handlers[].handler_name == 'cmdClose_Click'`
- **Angular Method**: Search for method in .component.ts
- **Logic Steps**: 1 steps
  1. Unload current form

### Validations

#### txtID Validation
- **Rule**: custom
- **Error Message**: "Client ID not found"
- **VB6 Source**: Lines Lines 87-88
- **Angular**: Validator in .component.ts

#### txtID Validation
- **Rule**: numeric
- **Error Message**: "Type mismatch"
- **VB6 Source**: Lines Line 85
- **Angular**: Validator in .component.ts

### Data Operations

#### SELECT Client
- **Method**: cmdOpen_Click()
- **VB6 Source**: Lines Line 85
- **Angular**: Service method (if service generated)

### Generation Notes

- **Complexity**: 2
- **Estimated Manual Effort**: 0.8 hours
- **Automation Rate**: 85%

**Notes**:
- Create interfaces/services for: Client, ClientEdit
- Fix 1 security issue(s)
- Use Angular Material Dialog for modal behavior
- Use Reactive Forms with validation