import { Component, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatDialog } from '@angular/material/dialog';
import { MatCardModule } from '@angular/material/card';

export interface Client {
  id?: number;
  [key: string]: any;
}

export interface ClientService {
  createNew(): Client;
  getById(id: number): Promise<Client | null>;
}

export interface ClientEditComponent {
  // Placeholder for ClientEdit component interface
}

@Component({
  selector: 'app-start',
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatCardModule
  ],
  templateUrl: './start.component.html',
  styleUrl: './start.component.scss'
})
export class StartFormComponent {
  private dialog = inject(MatDialog);
  private snackBar = inject(MatSnackBar);

  // VB6 txtID.Text equivalent - using signals (Angular 17)
  clientId = signal('');
  isLoading = signal(false);

  // Mock services - in real implementation these would be injected
  private clientService: ClientService = {
    createNew(): Client {
      return { id: 0 };
    },
    async getById(id: number): Promise<Client | null> {
      // Mock implementation - replace with actual service call
      if (id === 123) {
        return { id: 123, name: 'Test Client' };
      }
      return null;
    }
  };

  // VB6 cmdNew_Click() - Lines 71-78
  async onNewClick(): Promise<void> {
    try {
      // Create new Client object (Line 72)
      const objClient = this.clientService.createNew();
      
      // Create new ClientEdit form and show modal (Lines 73-76)
      // In real implementation, replace with actual ClientEditComponent
      const dialogRef = this.dialog.open(Object as any, {
        data: objClient,
        disableClose: true, // vbModal equivalent
        width: '600px'
      });

      await dialogRef.afterClosed().toPromise();
    } catch (error) {
      this.snackBar.open('Error creating new client', 'Close', { duration: 3000 });
    }
  }

  // VB6 cmdOpen_Click() - Lines 80-95
  async onOpenClick(): Promise<void> {
    const clientIdText = this.clientId().trim();
    
    if (!clientIdText) {
      this.snackBar.open('Please enter a Client ID', 'Close', { duration: 3000 });
      return;
    }

    // Validate numeric input (Line 85 - CLng conversion)
    const clientIdNum = Number(clientIdText);
    if (isNaN(clientIdNum) || !Number.isInteger(clientIdNum)) {
      this.snackBar.open('Client ID must be a valid number', 'Close', { duration: 3000 });
      return;
    }

    this.isLoading.set(true);

    try {
      // Get client by ID from database (Line 85)
      const objClient = await this.clientService.getById(clientIdNum);
      
      // Check if client was found (Lines 87-88)
      if (!objClient) {
        this.snackBar.open('Client ID not found', 'Close', { duration: 3000 });
        return;
      }

      // Show form with existing client (Line 91)
      const dialogRef = this.dialog.open(Object as any, {
        data: objClient,
        disableClose: true, // vbModal equivalent
        width: '600px'
      });

      await dialogRef.afterClosed().toPromise();
    } catch (error) {
      this.snackBar.open('Error retrieving client', 'Close', { duration: 3000 });
    } finally {
      this.isLoading.set(false);
    }
  }

  // VB6 cmdClose_Click() - Lines 97-98
  onCloseClick(): void {
    // In a dialog context, this would be: this.dialogRef.close()
    // In a routed context, this would be: this.router.navigate(['..'])
    window.close();
  }

  // Helper method for client ID input validation
  onClientIdChange(value: string): void {
    this.clientId.set(value);
  }
}