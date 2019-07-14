namespace Nu.Gaia.Design
{
    partial class StartForm
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
            this.binaryFilePathText = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.selectExecutable = new System.Windows.Forms.Button();
            this.customButton = new System.Windows.Forms.Button();
            this.defaultButton = new System.Windows.Forms.Button();
            this.openGameplayScreenCheckBox = new System.Windows.Forms.CheckBox();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.SuspendLayout();
            // 
            // binaryFilePathText
            // 
            this.binaryFilePathText.Location = new System.Drawing.Point(21, 69);
            this.binaryFilePathText.Name = "binaryFilePathText";
            this.binaryFilePathText.Size = new System.Drawing.Size(614, 26);
            this.binaryFilePathText.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(16, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(268, 20);
            this.label1.TabIndex = 1;
            this.label1.Text = "Select game\'s executable for editing.";
            // 
            // selectExecutable
            // 
            this.selectExecutable.Location = new System.Drawing.Point(644, 69);
            this.selectExecutable.Name = "selectExecutable";
            this.selectExecutable.Size = new System.Drawing.Size(36, 31);
            this.selectExecutable.TabIndex = 2;
            this.selectExecutable.Text = "...";
            this.selectExecutable.UseVisualStyleBackColor = true;
            this.selectExecutable.Click += new System.EventHandler(this.button1_Click);
            // 
            // customButton
            // 
            this.customButton.Location = new System.Drawing.Point(393, 217);
            this.customButton.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.customButton.Name = "customButton";
            this.customButton.Size = new System.Drawing.Size(144, 35);
            this.customButton.TabIndex = 3;
            this.customButton.Text = "Use Settings";
            this.customButton.UseVisualStyleBackColor = true;
            this.customButton.Click += new System.EventHandler(this.customButton_Click);
            // 
            // defaultButton
            // 
            this.defaultButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.defaultButton.Location = new System.Drawing.Point(546, 217);
            this.defaultButton.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.defaultButton.Name = "defaultButton";
            this.defaultButton.Size = new System.Drawing.Size(136, 35);
            this.defaultButton.TabIndex = 4;
            this.defaultButton.Text = "Use Defaults";
            this.defaultButton.UseVisualStyleBackColor = true;
            // 
            // openGameplayScreenCheckBox
            // 
            this.openGameplayScreenCheckBox.AutoSize = true;
            this.openGameplayScreenCheckBox.Location = new System.Drawing.Point(21, 140);
            this.openGameplayScreenCheckBox.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.openGameplayScreenCheckBox.Name = "openGameplayScreenCheckBox";
            this.openGameplayScreenCheckBox.Size = new System.Drawing.Size(205, 24);
            this.openGameplayScreenCheckBox.TabIndex = 5;
            this.openGameplayScreenCheckBox.Text = "Open Gameplay Screen";
            this.openGameplayScreenCheckBox.UseVisualStyleBackColor = true;
            // 
            // openFileDialog
            // 
            this.openFileDialog.Filter = "EXE files|*.exe|All files|*.*";
            // 
            // StartForm
            // 
            this.AcceptButton = this.customButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.defaultButton;
            this.ClientSize = new System.Drawing.Size(700, 275);
            this.ControlBox = false;
            this.Controls.Add(this.openGameplayScreenCheckBox);
            this.Controls.Add(this.defaultButton);
            this.Controls.Add(this.customButton);
            this.Controls.Add(this.selectExecutable);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.binaryFilePathText);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "StartForm";
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Editor Start Configuration";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button selectExecutable;
        public System.Windows.Forms.Button customButton;
        public System.Windows.Forms.Button defaultButton;
        public System.Windows.Forms.TextBox binaryFilePathText;
        public System.Windows.Forms.CheckBox openGameplayScreenCheckBox;
        public System.Windows.Forms.OpenFileDialog openFileDialog;
    }
}