import { Component, signal, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';
import { MatTableModule } from '@angular/material/table';
import { MatSnackBarModule, MatSnackBar } from '@angular/material/snackbar';
import { MatDialogModule, MatDialog } from '@angular/material/dialog';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';

interface Supplier {
  sid: number;
  sname: string;
  splace: string;
  smobno: string;
  smailid: string;
}

@Component({
  selector: 'app-frmsupplier',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatSelectModule,
    MatTableModule,
    MatSnackBarModule,
    MatDialogModule,
    MatCardModule,
    MatIconModule
  ],
  templateUrl: './frmsupplier.component.html',
  styleUrl: './frmsupplier.component.scss'
})
export class FrmsupplierComponent implements OnInit {
  private fb = inject(FormBuilder);
  private snackBar = inject(MatSnackBar);
  private dialog = inject(MatDialog);

  // Signals for state management
  suppliers = signal<Supplier[]>([]);
  searchOptions = signal<string[]>([]);
  selectedSupplier = signal<Supplier | null>(null);
  isLoading = signal(false);

  // Form setup
  supplierForm: FormGroup;
  searchControl = signal('');

  // Table configuration
  displayedColumns: string[] = ['sid', 'sname', 'splace', 'smobno', 'smailid', 'actions'];

  constructor() {
    // Initialize reactive form with validation (VB6 lines 100-103)
    this.supplierForm = this.fb.group({
      sname: ['', Validators.required],
      splace: ['', Validators.required],
      smobno: ['', Validators.required],
      smailid: ['', [Validators.required, Validators.email]]
    });
  }

  ngOnInit(): void {
    // VB6 Form_Load equivalent (lines 167-170)
    this.loadGrid();
    this.fillCombo();
  }

  // VB6 Command2_Click - Add new supplier (lines 95-113)
  onAddSupplier(): void {
    if (this.supplierForm.invalid) {
      // VB6 validation (lines 100-103)
      this.snackBar.open('All fields are required!', 'Close', {
        duration: 3000,
        panelClass: ['error-snackbar']
      });
      return;
    }

    this.isLoading.set(true);
    
    // Simulate database operation
    const newSupplier: Supplier = {
      sid: this.generateNewId(),
      sname: this.supplierForm.get('sname')?.value,
      splace: this.supplierForm.get('splace')?.value,
      smobno: this.supplierForm.get('smobno')?.value,
      smailid: this.supplierForm.get('smailid')?.value
    };

    // Add to suppliers array (simulating database insert)
    const currentSuppliers = this.suppliers();
    this.suppliers.set([...currentSuppliers, newSupplier]);

    // VB6 success message (line 112)
    this.snackBar.open('Supplier added successfully!', 'Close', {
      duration: 3000,
      panelClass: ['success-snackbar']
    });

    this.clearFields();
    this.fillCombo();
    this.isLoading.set(false);
  }

  // VB6 Command3_Click - Delete supplier (lines 115-127)
  onDeleteSupplier(supplier: Supplier): void {
    // VB6 confirmation dialog (lines 120-121)
    const confirmed = confirm('Are you sure you want to delete this supplier?');
    
    if (confirmed) {
      this.isLoading.set(true);
      
      // Remove from suppliers array (simulating database delete)
      const currentSuppliers = this.suppliers();
      const updatedSuppliers = currentSuppliers.filter(s => s.sid !== supplier.sid);
      this.suppliers.set(updatedSuppliers);

      this.snackBar.open('Supplier deleted successfully!', 'Close', {
        duration: 3000,
        panelClass: ['success-snackbar']
      });

      this.clearFields();
      this.fillCombo();
      this.isLoading.set(false);
    }
  }

  // VB6 Command4_Click - Edit supplier (lines 129-145)
  onEditSupplier(): void {
    const selected = this.selectedSupplier();
    if (!selected) {
      this.snackBar.open('Please select a supplier to edit', 'Close', {
        duration: 3000,
        panelClass: ['warning-snackbar']
      });
      return;
    }

    if (this.supplierForm.invalid) {
      this.snackBar.open('All fields are required!', 'Close', {
        duration: 3000,
        panelClass: ['error-snackbar']
      });
      return;
    }

    this.isLoading.set(true);

    // Update supplier in array (simulating database update)
    const currentSuppliers = this.suppliers();
    const updatedSuppliers = currentSuppliers.map(s => 
      s.sid === selected.sid ? {
        ...s,
        sname: this.supplierForm.get('sname')?.value,
        splace: this.supplierForm.get('splace')?.value,
        smobno: this.supplierForm.get('smobno')?.value,
        smailid: this.supplierForm.get('smailid')?.value
      } : s
    );

    this.suppliers.set(updatedSuppliers);

    this.snackBar.open('Supplier updated successfully!', 'Close', {
      duration: 3000,
      panelClass: ['success-snackbar']
    });

    this.clearFields();
    this.fillCombo();
    this.isLoading.set(false);
  }

  // VB6 Command5_Click - Reset form (lines 147-150)
  onResetForm(): void {
    this.clearFields();
  }

  // VB6 Combo1_Click - Search by name (lines 152-157)
  onSearchByName(): void {
    const searchTerm = this.searchControl();
    if (!searchTerm) {
      this.loadGrid();
      return;
    }

    // Filter suppliers by name (VB6 LIKE query simulation)
    const allSuppliers = this.getAllSuppliers();
    const filteredSuppliers = allSuppliers.filter(supplier =>
      supplier.sname.toLowerCase().includes(searchTerm.toLowerCase())
    );
    
    this.suppliers.set(filteredSuppliers);
  }

  // VB6 DataGrid1_Click - Select supplier (lines 159-165)
  onSelectSupplier(supplier: Supplier): void {
    this.selectedSupplier.set(supplier);
    
    // Populate form fields with selected supplier data
    this.supplierForm.patchValue({
      sname: supplier.sname,
      splace: supplier.splace,
      smobno: supplier.smobno,
      smailid: supplier.smailid
    });
  }

  // VB6 loadgrid method
  private loadGrid(): void {
    // Simulate loading all suppliers from database
    const allSuppliers = this.getAllSuppliers();
    this.suppliers.set(allSuppliers);
  }

  // VB6 fillcombo method
  private fillCombo(): void {
    // Populate search options with supplier names
    const allSuppliers = this.getAllSuppliers();
    const supplierNames = allSuppliers.map(s => s.sname);
    this.searchOptions.set([...new Set(supplierNames)]);
  }

  // VB6 clearfields method
  private clearFields(): void {
    this.supplierForm.reset();
    this.selectedSupplier.set(null);
    this.searchControl.set('');
  }

  // Utility methods
  private generateNewId(): number {
    const currentSuppliers = this.getAllSuppliers();
    return currentSuppliers.length > 0 ? Math.max(...currentSuppliers.map(s => s.sid)) + 1 : 1;
  }

  private getAllSuppliers(): Supplier[] {
    // Simulate database data - in real app this would be from a service
    return [
      { sid: 1, sname: 'ABC Supplies', splace: 'New York', smobno: '1234567890', smailid: 'abc@supplies.com' },
      { sid: 2, sname: 'XYZ Corp', splace: 'California', smobno: '0987654321', smailid: 'xyz@corp.com' },
      { sid: 3, sname: 'Global Traders', splace: 'Texas', smobno: '5555555555', smailid: 'global@traders.com' }
    ];
  }

  onSearchSelectionChange(selectedName: string): void {
    this.searchControl.set(selectedName);
    this.onSearchByName();
  }
}