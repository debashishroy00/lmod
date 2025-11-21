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

  it('should validate required fields on add supplier', () => {
    // VB6 validation test (lines 100-103)
    spyOn(component['snackBar'], 'open');
    
    component.onAddSupplier();
    
    expect(component['snackBar'].open).toHaveBeenCalledWith(
      'All fields are required!',
      'Close',
      jasmine.any(Object)
    );
  });

  it('should add supplier when form is valid', () => {
    // Fill form with valid data
    component.supplierForm.patchValue({
      sname: 'Test Supplier',
      splace: 'Test Location',
      smobno: '1234567890',
      smailid: 'test@supplier.com'
    });

    const initialCount = component.suppliers().length;
    spyOn(component['snackBar'], 'open');

    component.onAddSupplier();

    expect(component.suppliers().length).toBe(initialCount + 1);
    expect(component['snackBar'].open).toHaveBeenCalledWith(
      'Supplier added successfully!',
      'Close',
      jasmine.any(Object)
    );
  });

  it('should delete supplier with confirmation', () => {
    // VB6 delete test (lines 115-127)
    spyOn(window, 'confirm').and.returnValue(true);
    spyOn(component['snackBar'], 'open');

    const testSupplier = {
      sid: 1,
      sname: 'Test Supplier',
      splace: 'Test Location',
      smobno: '1234567890',
      smailid: 'test@supplier.com'
    };

    component.onDeleteSupplier(testSupplier);

    expect(window.confirm).toHaveBeenCalledWith(
      'Are you sure you want to delete this supplier?'
    );
    expect(component['snackBar'].open).toHaveBeenCalledWith(
      'Supplier deleted successfully!',
      'Close',
      jasmine.any(Object)
    );
  });

  it('should not delete supplier without confirmation', () => {
    spyOn(window, 'confirm').and.returnValue(false);
    const initialCount = component.suppliers().length;

    const testSupplier = {
      sid: 1,
      sname: 'Test Supplier',
      splace: 'Test Location',
      smobno: '1234567890',
      smailid: 'test@supplier.com'
    };

    component.onDeleteSupplier(testSupplier);

    expect(component.suppliers().length).toBe(initialCount);
  });

  it('should edit supplier when selected and form is valid', () => {
    // VB6 edit test (lines 129-145)
    const testSupplier = {
      sid: 1,
      sname: 'Original Name',
      splace: 'Original Location',
      smobno: '1234567890',
      smailid: 'original@supplier.com'
    };

    component.selectedSupplier.set(testSupplier);
    component.supplierForm.patchValue({
      sname: 'Updated Name',
      splace: 'Updated Location',
      smobno: '0987654321',
      smailid: 'updated@supplier.com'
    });

    spyOn(component['snackBar'], 'open');

    component.onEditSupplier();

    expect(component['snackBar'].open).toHaveBeenCalledWith(
      'Supplier updated successfully!',
      'Close',
      jasmine.any(Object)
    );
  });

  it('should show warning when editing without selection', () => {
    component.selectedSupplier.set(null);
    spyOn(component['snackBar'], 'open');

    component.onEditSupplier();

    expect(component['snackBar'].open).toHaveBeenCalledWith(
      'Please select a supplier to edit',
      'Close',
      jasmine.any(Object)
    );
  });

  it('should reset form fields', () => {
    // VB6 reset test (lines 147-150)
    component.supplierForm.patchValue({
      sname: 'Test',
      splace: 'Test',
      smobno: 'Test',
      smailid: 'test@test.com'
    });

    component.onResetForm();

    expect(component.supplierForm.get('sname')?.value).toBeNull();
    expect(component.supplierForm.get('splace')?.value).toBeNull();
    expect(component.supplierForm.get('smobno')?.value).toBeNull();
    expect(component.supplierForm.get('smailid')?.value).toBeNull();
  });

  it('should search suppliers by name', () => {
    // VB6 search test (lines 152-157)
    component.searchControl.set('ABC');
    
    component.onSearchByName();

    const filteredSuppliers = component.suppliers();
    expect(filteredSuppliers.every(s => 
      s.sname.toLowerCase().includes('abc')
    )).toBeTruthy();
  });

  it('should select supplier and populate form', () => {
    // VB6 grid click test (lines 159-165)
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

  it('should validate email format', () => {
    component.supplierForm.patchValue({
      sname: 'Test',
      splace: 'Test',
      smobno: '1234567890',
      smailid: 'invalid-email'
    });

    expect(component.supplierForm.get('smailid')?.hasError('email')).toBeTruthy();
  });

  it('should handle search selection change', () => {
    spyOn(component, 'onSearchByName');
    
    component.onSearchSelectionChange('Test Supplier');

    expect(component.searchControl()).toBe('Test Supplier');
    expect(component.onSearchByName).toHaveBeenCalled();
  });

  it('should generate new ID correctly', () => {
    const newId = component['generateNewId']();
    expect(newId).toBeGreaterThan(0);
    expect(typeof newId).toBe('number');
  });

  it('should fill combo with unique supplier names', () => {
    component['fillCombo']();
    const searchOptions = component.searchOptions();
    
    expect(searchOptions.length).toBeGreaterThan(0);
    expect(new Set(searchOptions).size).toBe(searchOptions.length);
  });
});