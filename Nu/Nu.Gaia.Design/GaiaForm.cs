using System.Windows.Forms;

namespace Nu.Gaia.Design
{
    public partial class GaiaForm : Form
    {
        public GaiaForm()
        {
            InitializeComponent();
            FormClosing += (_, __) => isClosing = true;
        }

        public bool IsClosing
        {
            get { return isClosing; }
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

        private bool isClosing;
    }
}
