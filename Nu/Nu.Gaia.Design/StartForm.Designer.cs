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
            binaryFilePathText = new TextBox();
            label1 = new Label();
            selectExecutable = new Button();
            customButton = new Button();
            defaultButton = new Button();
            openFileDialog = new OpenFileDialog();
            useImperativeExecutionCheckBox = new CheckBox();
            modeComboBox = new ComboBox();
            label2 = new Label();
            SuspendLayout();
            // 
            // binaryFilePathText
            // 
            binaryFilePathText.Location = new Point(16, 52);
            binaryFilePathText.Margin = new Padding(2);
            binaryFilePathText.Name = "binaryFilePathText";
            binaryFilePathText.Size = new Size(479, 23);
            binaryFilePathText.TabIndex = 1;
            // 
            // label1
            // 
            label1.AutoSize = true;
            label1.Location = new Point(13, 21);
            label1.Margin = new Padding(2, 0, 2, 0);
            label1.Name = "label1";
            label1.Size = new Size(200, 15);
            label1.TabIndex = 0;
            label1.Text = "Select game's executable for editing.";
            // 
            // selectExecutable
            // 
            selectExecutable.Location = new Point(500, 52);
            selectExecutable.Margin = new Padding(2);
            selectExecutable.Name = "selectExecutable";
            selectExecutable.Size = new Size(28, 23);
            selectExecutable.TabIndex = 2;
            selectExecutable.Text = "...";
            selectExecutable.UseVisualStyleBackColor = true;
            selectExecutable.Click += button1_Click;
            // 
            // customButton
            // 
            customButton.Location = new Point(306, 163);
            customButton.Margin = new Padding(4, 3, 4, 3);
            customButton.Name = "customButton";
            customButton.Size = new Size(112, 27);
            customButton.TabIndex = 6;
            customButton.Text = "Use Settings";
            customButton.UseVisualStyleBackColor = true;
            customButton.Click += customButton_Click;
            // 
            // defaultButton
            // 
            defaultButton.DialogResult = DialogResult.Cancel;
            defaultButton.Location = new Point(425, 163);
            defaultButton.Margin = new Padding(4, 3, 4, 3);
            defaultButton.Name = "defaultButton";
            defaultButton.Size = new Size(106, 27);
            defaultButton.TabIndex = 7;
            defaultButton.Text = "Use Defaults";
            defaultButton.UseVisualStyleBackColor = true;
            // 
            // openFileDialog
            // 
            openFileDialog.Filter = "DLL files|*.dll|All files|*.*";
            // 
            // useImperativeExecutionCheckBox
            // 
            useImperativeExecutionCheckBox.AutoSize = true;
            useImperativeExecutionCheckBox.Location = new Point(16, 126);
            useImperativeExecutionCheckBox.Margin = new Padding(4, 3, 4, 3);
            useImperativeExecutionCheckBox.Name = "useImperativeExecutionCheckBox";
            useImperativeExecutionCheckBox.Size = new Size(307, 19);
            useImperativeExecutionCheckBox.TabIndex = 5;
            useImperativeExecutionCheckBox.Text = "Use Imperative Execution (faster but no Undo / Redo)";
            useImperativeExecutionCheckBox.UseVisualStyleBackColor = true;
            // 
            // modeComboBox
            // 
            modeComboBox.DropDownStyle = ComboBoxStyle.DropDownList;
            modeComboBox.Enabled = false;
            modeComboBox.FormattingEnabled = true;
            modeComboBox.Location = new Point(65, 89);
            modeComboBox.Margin = new Padding(4, 3, 4, 3);
            modeComboBox.Name = "modeComboBox";
            modeComboBox.Size = new Size(465, 23);
            modeComboBox.TabIndex = 4;
            // 
            // label2
            // 
            label2.AutoSize = true;
            label2.Location = new Point(15, 93);
            label2.Margin = new Padding(4, 0, 4, 0);
            label2.Name = "label2";
            label2.Size = new Size(41, 15);
            label2.TabIndex = 3;
            label2.Text = "Mode:";
            // 
            // StartForm
            // 
            AcceptButton = customButton;
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            CancelButton = defaultButton;
            ClientSize = new Size(545, 207);
            ControlBox = false;
            Controls.Add(label2);
            Controls.Add(modeComboBox);
            Controls.Add(useImperativeExecutionCheckBox);
            Controls.Add(defaultButton);
            Controls.Add(customButton);
            Controls.Add(selectExecutable);
            Controls.Add(label1);
            Controls.Add(binaryFilePathText);
            Margin = new Padding(2);
            MaximizeBox = false;
            MinimizeBox = false;
            Name = "StartForm";
            SizeGripStyle = SizeGripStyle.Hide;
            StartPosition = FormStartPosition.CenterScreen;
            Text = "Editor Start Configuration";
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button selectExecutable;
        public System.Windows.Forms.Button customButton;
        public System.Windows.Forms.Button defaultButton;
        public System.Windows.Forms.TextBox binaryFilePathText;
        public System.Windows.Forms.OpenFileDialog openFileDialog;
        public System.Windows.Forms.CheckBox useImperativeExecutionCheckBox;
        private System.Windows.Forms.Label label2;
        public System.Windows.Forms.ComboBox modeComboBox;
    }
}