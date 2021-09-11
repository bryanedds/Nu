using System;
using System.Drawing;
using System.Windows.Forms;

namespace Nu.Gaia.Design
{
#if WINDOWS
    public class SelectablePanel : Panel
    {
        public SelectablePanel()
        {
            SetStyle(ControlStyles.Selectable, true);
            TabStop = true;
        }
        
        protected override void OnMouseDown(MouseEventArgs e)
        {
            Focus();
            base.OnMouseDown(e);
        }
        
        protected override bool IsInputKey(Keys keyData)
        {
            if (keyData == Keys.Up || keyData == Keys.Down) return true;
            if (keyData == Keys.Left || keyData == Keys.Right) return true;
            if (keyData == Keys.W || keyData == Keys.S) return true;
            if (keyData == Keys.A || keyData == Keys.D) return true;
            return base.IsInputKey(keyData);
        }
    }
#else
    public class SelectablePanel : UserControl
    {
        public SelectablePanel()
        {
            SetStyle(ControlStyles.Selectable, true);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!Focused) Focus();
            base.OnMouseDown(e);
        }
        
        protected override bool IsInputKey(Keys keyData)
        {
            if (keyData == Keys.Up || keyData == Keys.Down) return true;
            if (keyData == Keys.Left || keyData == Keys.Right) return true;
            if (keyData == Keys.W || keyData == Keys.S) return true;
            if (keyData == Keys.A || keyData == Keys.D) return true;
            return base.IsInputKey(keyData);
        }
    }
#endif
}
