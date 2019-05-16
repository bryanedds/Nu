using System;
using System.Diagnostics;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace Nu.Gaia.Design
{
    public partial class GaiaForm : Form
    {
        public GaiaForm()
        {
            InitializeComponent();
            proc = HookCallback;
            hookId = SetLowLevelKeyboardHook(proc);
            FormClosing += (_, __) => isClosing = true;
            FormClosed += (_, __) => UnhookWindowsHookEx(hookId);
        }

        public bool IsClosing
        {
            get { return isClosing; }
        }

        public IntPtr HookID
        {
            get { return hookId; }
        }

        public string propertyValueTextBoxText
        {
            get { return propertyValueTextBox.Text; }
            set
            {
                if (propertyValueTextBox.Text != value)
                    propertyValueTextBox.Text = value;
            }
        }

        public delegate IntPtr LowLevelKeyboardProc(int nCode, IntPtr wParam, IntPtr lParam);

        public event LowLevelKeyboardProc LowLevelKeyboardHook;

        public Control GetFocusedControl()
        {
            Control focusedControl = null;
            IntPtr focusedHandle = GetFocus();
            if (focusedHandle != IntPtr.Zero) focusedControl = Control.FromHandle(focusedHandle);
            return focusedControl;
        }

        private IntPtr SetLowLevelKeyboardHook(LowLevelKeyboardProc proc)
        {
            using (Process curProcess = Process.GetCurrentProcess())
                using (ProcessModule curModule = curProcess.MainModule)
                    return SetWindowsHookEx(WH_KEYBOARD_LL, proc, GetModuleHandle(curModule.ModuleName), 0);
        }

        private IntPtr HookCallback(int nCode, IntPtr wParam, IntPtr lParam)
        {
            try { LowLevelKeyboardHook?.Invoke(nCode, wParam, lParam); } catch { }
            return CallNextHookEx(hookId, nCode, wParam, lParam);
        }

        private const int WH_KEYBOARD_LL = 13;
        private readonly LowLevelKeyboardProc proc;
        private readonly IntPtr hookId;
        private bool isClosing;

        [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern IntPtr CallNextHookEx(IntPtr hhk, int nCode, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        private static extern IntPtr SetWindowsHookEx(int idHook, LowLevelKeyboardProc lpfn, IntPtr hMod, uint dwThreadId);

        [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool UnhookWindowsHookEx(IntPtr hhk);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        private static extern IntPtr GetModuleHandle(string lpModuleName);

        [DllImport("user32.dll", CharSet = CharSet.Auto, CallingConvention = CallingConvention.Winapi)]
        private static extern IntPtr GetFocus();
    }
}
