import React from 'react';
import ReactDOM from 'react-dom/client';
import { HashRouter, BrowserRouter, Routes, Route } from 'react-router-dom';
import './index.css';
import App from './App';
import Personal from './Personal';
import reportWebVitals from './reportWebVitals';

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <HashRouter>
    <Routes>
      <Route path="/" element={<App />} />
      <Route path="/:eventId/:gender/:forceAge?" element={<App />} />
      <Route path="/personal" element={<Personal />} />
      <Route path="/personal/:runnerId/" element={<Personal />} />
    </Routes>
  </HashRouter>
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
