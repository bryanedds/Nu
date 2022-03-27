namespace Nu.Gaia.Design
{
    partial class EntityPicker
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
			this.searchTextBox = new System.Windows.Forms.TextBox();
			this.label1 = new System.Windows.Forms.Label();
			this.okButton = new System.Windows.Forms.Button();
			this.cancelButton = new System.Windows.Forms.Button();
			this.entityListBox = new System.Windows.Forms.ListBox();
			this.changeAddressCheckBox = new System.Windows.Forms.CheckBox();
			this.SuspendLayout();
			// 
			// searchTextBox
			// 
			this.searchTextBox.Location = new System.Drawing.Point(12, 25);
			this.searchTextBox.Name = "searchTextBox";
			this.searchTextBox.Size = new System.Drawing.Size(310, 20);
			this.searchTextBox.TabIndex = 1;
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(12, 8);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(120, 13);
			this.label1.TabIndex = 0;
			this.label1.Text = "Search (Case-Sensitive)";
			// 
			// okButton
			// 
			this.okButton.Location = new System.Drawing.Point(122, 243);
			this.okButton.Name = "okButton";
			this.okButton.Size = new System.Drawing.Size(97, 29);
			this.okButton.TabIndex = 3;
			this.okButton.Text = "&OK";
			this.okButton.UseVisualStyleBackColor = true;
			// 
			// cancelButton
			// 
			this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.cancelButton.Location = new System.Drawing.Point(225, 244);
			this.cancelButton.Name = "cancelButton";
			this.cancelButton.Size = new System.Drawing.Size(97, 29);
			this.cancelButton.TabIndex = 4;
			this.cancelButton.Text = "&Cancel";
			this.cancelButton.UseVisualStyleBackColor = true;
			// 
			// entityListBox
			// 
			this.entityListBox.FormattingEnabled = true;
			this.entityListBox.Location = new System.Drawing.Point(12, 51);
			this.entityListBox.Name = "entityListBox";
			this.entityListBox.Size = new System.Drawing.Size(310, 186);
			this.entityListBox.TabIndex = 2;
			// 
			// changeAddressCheckBox
			// 
			this.changeAddressCheckBox.AutoSize = true;
			this.changeAddressCheckBox.Checked = true;
			this.changeAddressCheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
			this.changeAddressCheckBox.Location = new System.Drawing.Point(12, 249);
			this.changeAddressCheckBox.Name = "changeAddressCheckBox";
			this.changeAddressCheckBox.Size = new System.Drawing.Size(104, 17);
			this.changeAddressCheckBox.TabIndex = 5;
			this.changeAddressCheckBox.Text = "Change Address";
			this.changeAddressCheckBox.UseVisualStyleBackColor = true;
			// 
			// EntityPicker
			// 
			this.AcceptButton = this.okButton;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.cancelButton;
			this.ClientSize = new System.Drawing.Size(333, 278);
			this.Controls.Add(this.changeAddressCheckBox);
			this.Controls.Add(this.entityListBox);
			this.Controls.Add(this.cancelButton);
			this.Controls.Add(this.okButton);
			this.Controls.Add(this.label1);
			this.Controls.Add(this.searchTextBox);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
			this.Name = "EntityPicker";
			this.Text = "Pick Entity";
			this.ResumeLayout(false);
			this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.Label label1;
        public System.Windows.Forms.TextBox searchTextBox;
        public System.Windows.Forms.Button okButton;
        public System.Windows.Forms.Button cancelButton;
        public System.Windows.Forms.ListBox entityListBox;
		public System.Windows.Forms.CheckBox changeAddressCheckBox;
	}
}