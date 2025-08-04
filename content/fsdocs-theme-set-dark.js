const prefersDark = window.matchMedia("@media (prefers-color-scheme: dark)").matches;
let currentTheme = localStorage.getItem('theme') ?? (prefersDark ? 'dark' : 'light');
if (currentTheme === 'dark') {
    window.document.documentElement.setAttribute("data-theme", 'dark');
}
