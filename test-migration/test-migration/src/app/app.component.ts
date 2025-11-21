import { Component } from '@angular/core';
import { StartFormComponent } from './components/start/start.component';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [StartFormComponent],
  template: '<app-start></app-start>',
  styles: []
})
export class AppComponent {
  title = 'test-migration';
}
