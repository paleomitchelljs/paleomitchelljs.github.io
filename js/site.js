/* Shared site behaviour: mobile nav, theme toggle, nav scroll shadow.
   The initial theme is set by an inline snippet in each page's <head>
   to avoid a flash of the wrong theme before this file loads. */
(function () {
  'use strict';

  var root = document.documentElement;

  /* ---- Mobile nav ---- */
  var navToggle = document.querySelector('.nav-toggle');
  var navList = document.querySelector('nav ul');
  if (navToggle && navList) {
    navToggle.addEventListener('click', function () {
      var open = navList.classList.toggle('active');
      navToggle.setAttribute('aria-expanded', open ? 'true' : 'false');
    });
  }

  /* ---- Theme toggle ---- */
  function applyTheme(theme) {
    root.setAttribute('data-theme', theme);
    var toggle = document.querySelector('.theme-toggle');
    if (toggle) {
      toggle.setAttribute('aria-label',
        theme === 'dark' ? 'Switch to light theme' : 'Switch to dark theme');
    }
  }

  var themeToggle = document.querySelector('.theme-toggle');
  if (themeToggle) {
    themeToggle.addEventListener('click', function () {
      var next = root.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
      try { localStorage.setItem('theme', next); } catch (e) {}
      applyTheme(next);
    });
    // Keep the label in sync with whatever the head snippet set.
    applyTheme(root.getAttribute('data-theme') || 'light');
  }

  /* ---- Follow the system theme until the user makes an explicit choice ---- */
  if (window.matchMedia) {
    var media = window.matchMedia('(prefers-color-scheme: dark)');
    var onChange = function (e) {
      var stored;
      try { stored = localStorage.getItem('theme'); } catch (err) { stored = null; }
      if (!stored) applyTheme(e.matches ? 'dark' : 'light');
    };
    if (media.addEventListener) media.addEventListener('change', onChange);
    else if (media.addListener) media.addListener(onChange);
  }

  /* ---- Nav shadow on scroll ---- */
  var nav = document.querySelector('nav');
  if (nav) {
    var onScroll = function () {
      nav.classList.toggle('scrolled', window.scrollY > 8);
    };
    onScroll();
    window.addEventListener('scroll', onScroll, { passive: true });
  }
})();
