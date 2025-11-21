import { ComponentFixture, TestBed } from '@angular/core/testing';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { of } from 'rxjs';

import { StartFormComponent } from './start.component';

describe('StartFormComponent', () => {
  let component: StartFormComponent;
  let fixture: ComponentFixture<StartFormComponent>;
  let mockDialog: jasmine.SpyObj<MatDialog>;
  let mockSnackBar: jasmine.SpyObj<MatSnackBar>;

  beforeEach(async () => {
    const dialogSpy = jasmine.createSpyObj('MatDialog', ['open']);
    const snackBarSpy = jasmine.createSpyObj('MatSnackBar', ['open']);

    await TestBed.configureTestingModule({
      imports: [StartFormComponent, NoopAnimationsModule],
      providers: [
        { provide: MatDialog, useValue: dialogSpy },
        { provide: MatSnackBar, useValue: snackBarSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(StartFormComponent);
    component = fixture.componentInstance;
    mockDialog = TestBed.inject(MatDialog) as jasmine.SpyObj<MatDialog>;
    mockSnackBar = TestBed.inject(MatSnackBar) as jasmine.SpyObj<MatSnackBar>;
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with empty client ID', () => {
    expect(component.clientId()).toBe('');
    expect(component.isLoading()).toBe(false);
  });

  describe('onNewClick', () => {
    it('should open dialog with new client', async () => {
      const mockDialogRef = {
        afterClosed: () => of(null)
      };
      mockDialog.open.and.returnValue(mockDialogRef as any);

      await component.onNewClick();

      expect(mockDialog.open).toHaveBeenCalledWith(
        jasmine.any(Object),
        jasmine.objectContaining({
          data: jasmine.objectContaining({ id: 0 }),
          disableClose: true,
          width: '600px'
        })
      );
    });

    it('should show error message on failure', async () => {
      mockDialog.open.and.throwError('Dialog error');

      await component.onNewClick();

      expect(mockSnackBar.open).toHaveBeenCalledWith(
        'Error creating new client',
        'Close',
        { duration: 3000 }
      );
    });
  });

  describe('onOpenClick', () => {
    it('should show error when client ID is empty', async () => {
      component.clientId.set('');

      await component.onOpenClick();

      expect(mockSnackBar.open).toHaveBeenCalledWith(
        'Please enter a Client ID',
        'Close',
        { duration: 3000 }
      );
    });

    it('should show error when client ID is not numeric', async () => {
      component.clientId.set('abc');

      await component.onOpenClick();

      expect(mockSnackBar.open).toHaveBeenCalledWith(
        'Client ID must be a valid number',
        'Close',
        { duration: 3000 }
      );
    });

    it('should show error when client is not found', async () => {
      component.clientId.set('999');

      await component.onOpenClick();

      expect(mockSnackBar.open).toHaveBeenCalledWith(
        'Client ID not found',
        'Close',
        { duration: 3000 }
      );
    });

    it('should open dialog when client is found', async () => {
      const mockDialogRef = {
        afterClosed: () => of(null)
      };
      mockDialog.open.and.returnValue(mockDialogRef as any);
      component.clientId.set('123');

      await component.onOpenClick();

      expect(mockDialog.open).toHaveBeenCalledWith(
        jasmine.any(Object),
        jasmine.objectContaining({
          data: jasmine.objectContaining({ id: 123 }),
          disableClose: true,
          width: '600px'
        })
      );
    });

    it('should set loading state during operation', async () => {
      component.clientId.set('123');
      
      const openPromise = component.onOpenClick();
      
      // Should be loading during async operation
      expect(component.isLoading()).toBe(true);
      
      await openPromise;
      
      // Should not be loading after completion
      expect(component.isLoading()).toBe(false);
    });
  });

  describe('onCloseClick', () => {
    it('should call window.close', () => {
      spyOn(window, 'close');

      component.onCloseClick();

      expect(window.close).toHaveBeenCalled();
    });
  });

  describe('onClientIdChange', () => {
    it('should update client ID signal', () => {
      const testValue = '12345';

      component.onClientIdChange(testValue);

      expect(component.clientId()).toBe(testValue);
    });
  });

  describe('template integration', () => {
    it('should render client ID input field', () => {
      const compiled = fixture.nativeElement as HTMLElement;
      const input = compiled.querySelector('input[matInput]');
      
      expect(input).toBeTruthy();
      expect(input?.getAttribute('placeholder')).toBe('Enter Client ID');
    });

    it('should render all three action buttons', () => {
      const compiled = fixture.nativeElement as HTMLElement;
      const buttons = compiled.querySelectorAll('.action-button');
      
      expect(buttons.length).toBe(3);
      expect(buttons[0].textContent?.trim()).toBe('New');
      expect(buttons[1].textContent?.trim()).toBe('Open');
      expect(buttons[2].textContent?.trim()).toBe('Close');
    });

    it('should disable Open button when client ID is empty', () => {
      component.clientId.set('');
      fixture.detectChanges();

      const compiled = fixture.nativeElement as HTMLElement;
      const openButton = compiled.querySelectorAll('.action-button')[1] as HTMLButtonElement;
      
      expect(openButton.disabled).toBe(true);
    });

    it('should enable Open button when client ID has value', () => {
      component.clientId.set('123');
      fixture.detectChanges();

      const compiled = fixture.nativeElement as HTMLElement;
      const openButton = compiled.querySelectorAll('.action-button')[1] as HTMLButtonElement;
      
      expect(openButton.disabled).toBe(false);
    });
  });
});