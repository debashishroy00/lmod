import { Component, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { StartFormComponent } from './components/start/start.component';
import { FrmsupplierComponent } from './components/supplier/frmsupplier.component';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [CommonModule, MatButtonToggleModule, StartFormComponent, FrmsupplierComponent],
  template: `
    <div style="padding: 20px;">
      <h1>VB6 → Angular Migration Demo</h1>
      <mat-button-toggle-group [(value)]="selectedForm" style="margin-bottom: 20px;">
        <mat-button-toggle value="start">StartForm (Simple - 5 controls)</mat-button-toggle>
        <mat-button-toggle value="supplier">SupplierForm (Medium - 16 controls)</mat-button-toggle>
      </mat-button-toggle-group>

      <app-start *ngIf="selectedForm === 'start'"></app-start>
      <app-frmsupplier *ngIf="selectedForm === 'supplier'"></app-frmsupplier>
    </div>
  `,
  styles: []
})
export class AppComponent {
  title = 'test-migration';
  selectedForm = 'start';
}
