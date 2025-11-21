import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MatSnackBarModule } from '@angular/material/snackbar';
import { MatDialogModule } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { FrmsupplierComponent } from './frmsupplier.component';

describe('FrmsupplierComponent', () => {
  let component: FrmsupplierComponent;
  let fixture: ComponentFixture<FrmsupplierComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [
        FrmsupplierComponent,
        ReactiveFormsModule,
        MatSnackBarModule,
        MatDialogModule,
        BrowserAnimationsModule
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(FrmsupplierComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with empty values', () => {
    expect(component.supplierForm.get('sname')?.value).toBe('');
    expect(component.supplierForm.get('splace')?.value).toBe('');
    expect(component.supplierForm.get('smobno')?.value).toBe('');
    expect(component.supplierForm.get('smailid')?.value).toBe('');
  });

  it('should load suppliers on init', () => {
    component.ngOnInit();
    expect(component.suppliers().length).toBeGreaterThan(0);
  });

  it('should validate required fields', () => {
    // Test VB6 validation (lines 100-103)
    component.supplierForm.patchValue({
      sname: '',
      splace: '',
      smobno: '',
      smailid: ''
    });

    expect(component.supplierForm.invalid).toBeTruthy();
    expect(component.supplierForm.get('sname')?.hasError('required')).toBeTruthy();
    expect(component.supplierForm.get('splace')?.hasError('required')).toBeTruthy();
    expect(component.supplierForm.get('smobno')?.hasError('required')).toBeTruthy();
    expect(component.supplierForm.get('smailid')?.hasError('required')).toBeTruthy();
  });

  it('should validate email format', () => {
    component.supplierForm.patchValue({
      smailid: 'invalid-email'
    });

    expect(component.supplierForm.get('smailid')?.hasError('email')).toBeTruthy();
  });

  it('should add supplier when form is valid', () => {
    // Test VB6 Command2_Click (lines 95-113)
    const initialCount = component.suppliers().length;
    
    component.supplierForm.patchValue({
      sname: 'Test Supplier',
      splace: 'Test Location',
      smobno: '1234567890',
      smailid: 'test@supplier.com'
    });

    component.onAddSupplier();

    expect(component.suppliers().length).toBe(initialCount + 1);
    expect(component.supplierForm.pristine).toBeTruthy();
  });

  it('should not add supplier when form is invalid', () => {
    // Test VB6 validation error handling
    const initialCount = component.suppliers().length;
    
    component.supplierForm.patchValue({
      sname: '',
      splace: '',
      smobno: '',
      smailid: ''
    });

    component.onAddSupplier();

    expect(component.suppliers().length).toBe(initialCount);
  });

  it('should select supplier and populate form', () => {
    // Test VB6 DataGrid1_Click (lines 159-165)
    const testSupplier = {
      sid: 1,
      sname: 'Test Supplier',
      splace: 'Test Location',
      smobno: '1234567890',
      smailid: 'test@supplier.com'
    };

    component.onSelectSupplier(testSupplier);

    expect(component.selectedSupplier()).toEqual(testSupplier);
    expect(component.supplierForm.get('sname')?.value).toBe('Test Supplier');
    expect(component.supplierForm.get('splace')?.value).toBe('Test Location');
    expect(component.supplierForm.get('smobno')?.value).toBe('1234567890');
    expect(component.supplierForm.get('smailid')?.value).toBe('test@supplier.com');
  });

  it('should delete supplier', () => {
    // Test VB6 Command3_Click (lines 115-127)
    spyOn(window, 'confirm').and.returnValue(true);
    
    const testSupplier = {
      sid: 999,
      sname: 'To Delete',
      splace: 'Test Location',
      smobno: '1234567890',
      smailid: 'delete@test.com'
    };

    // Add supplier first
    const currentSuppliers = component.suppliers();
    component.suppliers.set([...currentSuppliers, testSupplier]);
    const countAfterAdd = component.suppliers().length;

    // Delete supplier
    component.onDeleteSupplier(testSupplier);

    expect(component.suppliers().length).toBe(countAfterAdd - 1);
    expect(component.suppliers().find(s => s.sid === 999)).toBeUndefined();
  });

  it('should not delete supplier when user cancels', () => {
    // Test VB6 confirmation dialog cancellation
    spyOn(window, 'confirm').and.returnValue(false);
    
    const initialCount = component.suppliers().length;
    const testSupplier = component.suppliers()[0];

    component.onDeleteSupplier(testSupplier);

    expect(component.suppliers().length).toBe(initialCount);
  });

  it('should update supplier when selected and form is valid', () => {
    // Test VB6 Command4_Click (lines 129-145)
    const testSupplier = {
      sid: 1,
      sname: 'Original Name',
      splace: 'Original Location',
      smobno: '1111111111',
      smailid: 'original@test.com'
    };

    component.selectedSupplier.set(testSupplier);
    component.suppliers.set([testSupplier]);

    component.supplierForm.patchValue({
      sname: 'Updated Name',
      splace: 'Updated Location',
      smobno: '2222222222',
      smailid: 'updated@test.com'
    });

    component.onEditSupplier();

    const updatedSupplier = component.suppliers().find(s => s.sid === 1);
    expect(updatedSupplier?.sname).toBe('Updated Name');
    expect(updatedSupplier?.splace).toBe('Updated Location');
    expect(updatedSupplier?.smobno).toBe('2222222222');
    expect(updatedSupplier?.smailid).toBe('updated@test.com');
  });

  it('should reset form', () => {
    // Test VB6 Command5_Click (lines 147-150)
    component.supplierForm.patchValue({
      sname: 'Test',
      splace: 'Test',
      smobno: 'Test',
      smailid: 'test@test.com'
    });

    const testSupplier = { sid: 1, sname: 'Test', splace: 'Test', smobno: 'Test', smailid: 'test@test.com' };
    component.selectedSupplier.set(testSupplier);
    component.searchControl.set('Test Search');

    component.onResetForm();

    expect(component.supplierForm.get('sname')?.value).toBe('');
    expect(component.supplierForm.get('splace')?.value).toBe('');
    expect(component.supplierForm.get('smobno')?.value).toBe('');
    expect(component.supplierForm.get('smailid')?.value).toBe('');
    expect(component.selectedSupplier()).toBeNull();
    expect(component.searchControl()).toBe('');
  });

  it('should search suppliers by name', () => {
    // Test VB6 Combo1_Click (lines 152-157)
    component.searchControl.set('ABC');
    component.onSearchByName();

    const filteredSuppliers = component.suppliers();
    expect(filteredSuppliers.every(s => s.sname.toLowerCase().includes('abc'))).toBeTruthy();
  });

  it('should show all suppliers when search is cleared', () => {
    // Test search reset functionality
    component.searchControl.set('');
    component.onSearchByName();

    expect(component.suppliers().length).toBeGreaterThan(0);
  });

  it('should populate search options', () => {
    component.ngOnInit();
    expect(component.searchOptions().length).toBeGreaterThan(0);
  });

  it('should handle search selection change', () => {
    const testName = 'ABC Supplies';
    component.onSearchSelectionChange(testName);

    expect(component.searchControl()).toBe(testName);
  });

  it('should generate new ID correctly', () => {
    const newId = (component as any).generateNewId();
    expect(typeof newId).toBe('number');
    expect(newId).toBeGreaterThan(0);
  });
});