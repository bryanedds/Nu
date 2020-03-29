// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

using System;
using System.Windows.Forms;

namespace Nu.Gaia.Design
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new GaiaForm());
        }
    }
}
