using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Nu.Gaia.Design
{
    public partial class GaiaForm : Form
    {
        public GaiaForm()
        {
            InitializeComponent();
            Load += GaiaForm_Load;
        }

        private void GaiaForm_Load(object sender, EventArgs e)
        {
            // HACK: next 2 lines solves this bug - http://stackoverflow.com/a/3679036/1082782
            propertyValueTextBox.AutoWordSelection = true;
            propertyValueTextBox.AutoWordSelection = false;
        }
    }
}
