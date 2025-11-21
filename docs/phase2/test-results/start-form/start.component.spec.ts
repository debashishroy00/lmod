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
    it('should create new client and open dialog', async () => {
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
      spyOn(component['clientService'], 'getById').and.returnValue(Promise.resolve(null));

      await component.onOpenClick();

      expect(mockSnackBar.open).toHaveBeenCalledWith(
        'Client ID not found',
        'Close',
        { duration: 3000 }
      );
    });

    it('should open dialog when client is found', async () => {
      const mockClient = { id: 123, name: 'Test Client' };
      const mockDialogRef = {
        afterClosed: () => of(null)
      };
      
      component.clientId.set('123');
      spyOn(component['clientService'], 'getById').and.returnValue(Promise.resolve(mockClient));
      mockDialog.open.and.returnValue(mockDialogRef as any);

      await component.onOpenClick();

      expect(mockDialog.open).toHaveBeenCalledWith(
        jasmine.any(Object),
        jasmine.objectContaining({
          data: mockClient,
          disableClose: true,
          width: '600px'
        })
      );
    });

    it('should handle service errors gracefully', async () => {
      component.clientId.set('123');
      spyOn(component['clientService'], 'getById').and.returnValue(Promise.reject('Service error'));

      await component.onOpenClick();

      expect(mockSnackBar.open).toHaveBeenCalledWith(
        'Error retrieving client',
        'Close',
        { duration: 3000 }
      );
    });

    it('should set loading state during operation', async () => {
      component.clientId.set('123');
      spyOn(component['clientService'], 'getById').and.returnValue(
        new Promise(resolve => setTimeout(() => resolve(null), 100))
      );

      const promise = component.onOpenClick();
      expect(component.isLoading()).toBe(true);

      await promise;
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
    it('should render all buttons', () => {
      const buttons = fixture.nativeElement.querySelectorAll('button');
      expect(buttons.length).toBe(3);
      
      const buttonTexts = Array.from(buttons).map((btn: any) => btn.textContent.trim());
      expect(buttonTexts).toContain('New');
      expect(buttonTexts).toContain('Open');
      expect(buttonTexts).toContain('Close');
    });

    it('should render client ID input field', () => {
      const input = fixture.nativeElement.querySelector('input[matInput]');
      expect(input).toBeTruthy();
      expect(input.placeholder).toBe('Enter Client ID');
    });

    it('should disable Open button when client ID is empty', () => {
      component.clientId.set('');
      fixture.detectChanges();

      const openButton = fixture.nativeElement.querySelector('button:nth-of-type(2)');
      expect(openButton.disabled).toBe(true);
    });

    it('should enable Open button when client ID has value', () => {
      component.clientId.set('123');
      fixture.detectChanges();

      const openButton = fixture.nativeElement.querySelector('button:nth-of-type(2)');
      expect(openButton.disabled).toBe(false);
    });
  });
});