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
            _proc = HookCallback;
            _hookID = SetHook(_proc);
            FormClosing += (_, __) => isClosing = true;
            FormClosed += (_, __) => UnhookWindowsHookEx(_hookID);
        }

        public bool IsClosing
        {
            get { return isClosing; }
        }

        public IntPtr HookID
        {
            get { return _hookID; }
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

        public IntPtr SetHook(LowLevelKeyboardProc proc)
        {
            using (Process curProcess = Process.GetCurrentProcess())
            using (ProcessModule curModule = curProcess.MainModule)
            {
                return SetWindowsHookEx(WH_KEYBOARD_LL, proc, GetModuleHandle(curModule.ModuleName), 0);
            }
        }

        public Control GetFocusedControl()
        {
            Control focusedControl = null;
            IntPtr focusedHandle = GetFocus();
            if (focusedHandle != IntPtr.Zero) focusedControl = Control.FromHandle(focusedHandle);
            return focusedControl;
        }

        private IntPtr HookCallback(int nCode, IntPtr wParam, IntPtr lParam)
        {
            return CallNextHookEx(_hookID, nCode, wParam, lParam);
        }

        private const int WH_KEYBOARD_LL = 13;
        private readonly LowLevelKeyboardProc _proc;
        private readonly IntPtr _hookID = IntPtr.Zero;
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
