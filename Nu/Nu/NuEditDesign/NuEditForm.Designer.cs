namespace NuEditDesign
{
    partial class NuEditForm
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(NuEditForm));
            this.menuStrip = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.newToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.saveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.undoToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.redoToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripSeparator();
            this.cutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.copyToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.pasteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem4 = new System.Windows.Forms.ToolStripSeparator();
            this.createToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.deleteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.displayPanel = new System.Windows.Forms.Panel();
            this.contextMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.createContextMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.deleteContextMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripSeparator();
            this.cutContextMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.copyContextMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.pasteContextMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.propertyGrid = new System.Windows.Forms.PropertyGrid();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.toolStrip = new System.Windows.Forms.ToolStrip();
            this.undoButton = new System.Windows.Forms.ToolStripButton();
            this.redoButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
            this.positionSnapLabel = new System.Windows.Forms.ToolStripLabel();
            this.positionSnapTextBox = new System.Windows.Forms.ToolStripTextBox();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.rotationSnapLabel = new System.Windows.Forms.ToolStripLabel();
            this.rotationSnapTextBox = new System.Windows.Forms.ToolStripTextBox();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.createEntityButton = new System.Windows.Forms.ToolStripButton();
            this.createEntityComboBox = new System.Windows.Forms.ToolStripComboBox();
            this.creationDepthLabel = new System.Windows.Forms.ToolStripLabel();
            this.creationDepthTextBox = new System.Windows.Forms.ToolStripTextBox();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.deleteEntityButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.quickSizeToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.resetCameraButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator7 = new System.Windows.Forms.ToolStripSeparator();
            this.interactButton = new System.Windows.Forms.ToolStripButton();
            this.menuStrip.SuspendLayout();
            this.contextMenuStrip.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.toolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip
            // 
            this.menuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.editToolStripMenuItem});
            this.menuStrip.Location = new System.Drawing.Point(0, 0);
            this.menuStrip.Name = "menuStrip";
            this.menuStrip.Size = new System.Drawing.Size(1272, 26);
            this.menuStrip.TabIndex = 0;
            this.menuStrip.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newToolStripMenuItem,
            this.openToolStripMenuItem,
            this.saveToolStripMenuItem,
            this.toolStripMenuItem1,
            this.exitToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(40, 22);
            this.fileToolStripMenuItem.Text = "&File";
            // 
            // newToolStripMenuItem
            // 
            this.newToolStripMenuItem.Name = "newToolStripMenuItem";
            this.newToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.N)));
            this.newToolStripMenuItem.Size = new System.Drawing.Size(177, 22);
            this.newToolStripMenuItem.Text = "&New";
            // 
            // openToolStripMenuItem
            // 
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.openToolStripMenuItem.Size = new System.Drawing.Size(177, 22);
            this.openToolStripMenuItem.Text = "&Open...";
            // 
            // saveToolStripMenuItem
            // 
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.S)));
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(177, 22);
            this.saveToolStripMenuItem.Text = "&Save...";
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(174, 6);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(177, 22);
            this.exitToolStripMenuItem.Text = "E&xit";
            // 
            // editToolStripMenuItem
            // 
            this.editToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.undoToolStripMenuItem,
            this.redoToolStripMenuItem,
            this.toolStripMenuItem2,
            this.cutToolStripMenuItem,
            this.copyToolStripMenuItem,
            this.pasteToolStripMenuItem,
            this.toolStripMenuItem4,
            this.createToolStripMenuItem,
            this.deleteToolStripMenuItem});
            this.editToolStripMenuItem.Name = "editToolStripMenuItem";
            this.editToolStripMenuItem.Size = new System.Drawing.Size(43, 22);
            this.editToolStripMenuItem.Text = "&Edit";
            // 
            // undoToolStripMenuItem
            // 
            this.undoToolStripMenuItem.Name = "undoToolStripMenuItem";
            this.undoToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Z)));
            this.undoToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.undoToolStripMenuItem.Text = "&Undo";
            // 
            // redoToolStripMenuItem
            // 
            this.redoToolStripMenuItem.Name = "redoToolStripMenuItem";
            this.redoToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Y)));
            this.redoToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.redoToolStripMenuItem.Text = "&Redo";
            // 
            // toolStripMenuItem2
            // 
            this.toolStripMenuItem2.Name = "toolStripMenuItem2";
            this.toolStripMenuItem2.Size = new System.Drawing.Size(164, 6);
            // 
            // cutToolStripMenuItem
            // 
            this.cutToolStripMenuItem.Name = "cutToolStripMenuItem";
            this.cutToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.X)));
            this.cutToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.cutToolStripMenuItem.Text = "C&ut";
            // 
            // copyToolStripMenuItem
            // 
            this.copyToolStripMenuItem.Name = "copyToolStripMenuItem";
            this.copyToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.C)));
            this.copyToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.copyToolStripMenuItem.Text = "&Copy";
            // 
            // pasteToolStripMenuItem
            // 
            this.pasteToolStripMenuItem.Name = "pasteToolStripMenuItem";
            this.pasteToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.V)));
            this.pasteToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.pasteToolStripMenuItem.Text = "&Paste";
            // 
            // toolStripMenuItem4
            // 
            this.toolStripMenuItem4.Name = "toolStripMenuItem4";
            this.toolStripMenuItem4.Size = new System.Drawing.Size(164, 6);
            // 
            // createToolStripMenuItem
            // 
            this.createToolStripMenuItem.Name = "createToolStripMenuItem";
            this.createToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.E)));
            this.createToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.createToolStripMenuItem.Text = "Cr&eate";
            // 
            // deleteToolStripMenuItem
            // 
            this.deleteToolStripMenuItem.Name = "deleteToolStripMenuItem";
            this.deleteToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.D)));
            this.deleteToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.deleteToolStripMenuItem.Text = "&Delete";
            // 
            // displayPanel
            // 
            this.displayPanel.ContextMenuStrip = this.contextMenuStrip;
            this.displayPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.displayPanel.Location = new System.Drawing.Point(0, 0);
            this.displayPanel.Name = "displayPanel";
            this.displayPanel.Size = new System.Drawing.Size(1001, 662);
            this.displayPanel.TabIndex = 1;
            // 
            // contextMenuStrip
            // 
            this.contextMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.createContextMenuItem,
            this.deleteContextMenuItem,
            this.toolStripMenuItem3,
            this.cutContextMenuItem,
            this.copyContextMenuItem,
            this.pasteContextMenuItem});
            this.contextMenuStrip.Name = "contextMenuStrip";
            this.contextMenuStrip.Size = new System.Drawing.Size(132, 120);
            // 
            // createContextMenuItem
            // 
            this.createContextMenuItem.Name = "createContextMenuItem";
            this.createContextMenuItem.Size = new System.Drawing.Size(131, 22);
            this.createContextMenuItem.Text = "C[&r]eate";
            // 
            // deleteContextMenuItem
            // 
            this.deleteContextMenuItem.Name = "deleteContextMenuItem";
            this.deleteContextMenuItem.Size = new System.Drawing.Size(131, 22);
            this.deleteContextMenuItem.Text = "[&D]elete";
            // 
            // toolStripMenuItem3
            // 
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(128, 6);
            // 
            // cutContextMenuItem
            // 
            this.cutContextMenuItem.Name = "cutContextMenuItem";
            this.cutContextMenuItem.Size = new System.Drawing.Size(131, 22);
            this.cutContextMenuItem.Text = "C[&u]t";
            // 
            // copyContextMenuItem
            // 
            this.copyContextMenuItem.Name = "copyContextMenuItem";
            this.copyContextMenuItem.Size = new System.Drawing.Size(131, 22);
            this.copyContextMenuItem.Text = "[&C]opy";
            // 
            // pasteContextMenuItem
            // 
            this.pasteContextMenuItem.Name = "pasteContextMenuItem";
            this.pasteContextMenuItem.Size = new System.Drawing.Size(131, 22);
            this.pasteContextMenuItem.Text = "[&P]aste";
            // 
            // propertyGrid
            // 
            this.propertyGrid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.propertyGrid.Location = new System.Drawing.Point(0, 0);
            this.propertyGrid.Name = "propertyGrid";
            this.propertyGrid.Size = new System.Drawing.Size(267, 662);
            this.propertyGrid.TabIndex = 2;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 26);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.displayPanel);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.propertyGrid);
            this.splitContainer1.Size = new System.Drawing.Size(1272, 662);
            this.splitContainer1.SplitterDistance = 1001;
            this.splitContainer1.TabIndex = 3;
            // 
            // openFileDialog
            // 
            this.openFileDialog.DefaultExt = "nugroup";
            this.openFileDialog.FileName = "*.nugroup";
            // 
            // saveFileDialog
            // 
            this.saveFileDialog.DefaultExt = "nugroup";
            this.saveFileDialog.FileName = "Document.nugroup";
            // 
            // toolStrip
            // 
            this.toolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.toolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.undoButton,
            this.redoButton,
            this.toolStripSeparator6,
            this.positionSnapLabel,
            this.positionSnapTextBox,
            this.toolStripSeparator1,
            this.rotationSnapLabel,
            this.rotationSnapTextBox,
            this.toolStripSeparator2,
            this.createEntityButton,
            this.createEntityComboBox,
            this.creationDepthLabel,
            this.creationDepthTextBox,
            this.toolStripSeparator3,
            this.quickSizeToolStripButton,
            this.toolStripSeparator4,
            this.deleteEntityButton,
            this.toolStripSeparator5,
            this.resetCameraButton,
            this.toolStripSeparator7,
            this.interactButton});
            this.toolStrip.Location = new System.Drawing.Point(93, 0);
            this.toolStrip.Name = "toolStrip";
            this.toolStrip.Size = new System.Drawing.Size(1073, 26);
            this.toolStrip.TabIndex = 4;
            this.toolStrip.Text = "toolStrip1";
            // 
            // undoButton
            // 
            this.undoButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.undoButton.Image = ((System.Drawing.Image)(resources.GetObject("undoButton.Image")));
            this.undoButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.undoButton.Name = "undoButton";
            this.undoButton.Size = new System.Drawing.Size(46, 23);
            this.undoButton.Text = "&Undo";
            // 
            // redoButton
            // 
            this.redoButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.redoButton.Image = ((System.Drawing.Image)(resources.GetObject("redoButton.Image")));
            this.redoButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.redoButton.Name = "redoButton";
            this.redoButton.Size = new System.Drawing.Size(45, 23);
            this.redoButton.Text = "&Redo";
            // 
            // toolStripSeparator6
            // 
            this.toolStripSeparator6.Name = "toolStripSeparator6";
            this.toolStripSeparator6.Size = new System.Drawing.Size(6, 26);
            // 
            // positionSnapLabel
            // 
            this.positionSnapLabel.Name = "positionSnapLabel";
            this.positionSnapLabel.Size = new System.Drawing.Size(93, 23);
            this.positionSnapLabel.Text = "Position Snap";
            // 
            // positionSnapTextBox
            // 
            this.positionSnapTextBox.Name = "positionSnapTextBox";
            this.positionSnapTextBox.Size = new System.Drawing.Size(26, 26);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 26);
            // 
            // rotationSnapLabel
            // 
            this.rotationSnapLabel.Name = "rotationSnapLabel";
            this.rotationSnapLabel.Size = new System.Drawing.Size(98, 23);
            this.rotationSnapLabel.Text = "Rotation Snap";
            // 
            // rotationSnapTextBox
            // 
            this.rotationSnapTextBox.Name = "rotationSnapTextBox";
            this.rotationSnapTextBox.Size = new System.Drawing.Size(26, 26);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 26);
            // 
            // createEntityButton
            // 
            this.createEntityButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.createEntityButton.Image = ((System.Drawing.Image)(resources.GetObject("createEntityButton.Image")));
            this.createEntityButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.createEntityButton.Name = "createEntityButton";
            this.createEntityButton.Size = new System.Drawing.Size(96, 23);
            this.createEntityButton.Text = "&Create Entity";
            // 
            // createEntityComboBox
            // 
            this.createEntityComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.createEntityComboBox.Name = "createEntityComboBox";
            this.createEntityComboBox.Size = new System.Drawing.Size(121, 26);
            // 
            // creationDepthLabel
            // 
            this.creationDepthLabel.Name = "creationDepthLabel";
            this.creationDepthLabel.Size = new System.Drawing.Size(65, 23);
            this.creationDepthLabel.Text = "at Depth";
            // 
            // creationDepthTextBox
            // 
            this.creationDepthTextBox.Name = "creationDepthTextBox";
            this.creationDepthTextBox.Size = new System.Drawing.Size(26, 26);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(6, 26);
            // 
            // deleteEntityButton
            // 
            this.deleteEntityButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.deleteEntityButton.Image = ((System.Drawing.Image)(resources.GetObject("deleteEntityButton.Image")));
            this.deleteEntityButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.deleteEntityButton.Name = "deleteEntityButton";
            this.deleteEntityButton.Size = new System.Drawing.Size(94, 23);
            this.deleteEntityButton.Text = "&Delete Entity";
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(6, 26);
            // 
            // quickSizeToolStripButton
            // 
            this.quickSizeToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.quickSizeToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("quickSizeToolStripButton.Image")));
            this.quickSizeToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.quickSizeToolStripButton.Name = "quickSizeToolStripButton";
            this.quickSizeToolStripButton.Size = new System.Drawing.Size(77, 23);
            this.quickSizeToolStripButton.Text = "&Quick Size";
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(6, 26);
            // 
            // resetCameraButton
            // 
            this.resetCameraButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.resetCameraButton.Image = ((System.Drawing.Image)(resources.GetObject("resetCameraButton.Image")));
            this.resetCameraButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.resetCameraButton.Name = "resetCameraButton";
            this.resetCameraButton.Size = new System.Drawing.Size(105, 23);
            this.resetCameraButton.Text = "&Reset Camera";
            // 
            // toolStripSeparator7
            // 
            this.toolStripSeparator7.Name = "toolStripSeparator7";
            this.toolStripSeparator7.Size = new System.Drawing.Size(6, 26);
            // 
            // interactButton
            // 
            this.interactButton.CheckOnClick = true;
            this.interactButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.interactButton.Image = ((System.Drawing.Image)(resources.GetObject("interactButton.Image")));
            this.interactButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.interactButton.Name = "interactButton";
            this.interactButton.Size = new System.Drawing.Size(64, 23);
            this.interactButton.Text = "&Interact";
            // 
            // NuEditForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1272, 688);
            this.Controls.Add(this.toolStrip);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.menuStrip);
            this.MainMenuStrip = this.menuStrip;
            this.Name = "NuEditForm";
            this.Text = "NuEdit";
            this.menuStrip.ResumeLayout(false);
            this.menuStrip.PerformLayout();
            this.contextMenuStrip.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.toolStrip.ResumeLayout(false);
            this.toolStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        public System.Windows.Forms.MenuStrip menuStrip;
        public System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem saveToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        public System.Windows.Forms.Panel displayPanel;
        public System.Windows.Forms.PropertyGrid propertyGrid;
        public System.Windows.Forms.OpenFileDialog openFileDialog;
        public System.Windows.Forms.SaveFileDialog saveFileDialog;
        public System.Windows.Forms.ToolStripSeparator toolStripMenuItem2;
        public System.Windows.Forms.ToolStripMenuItem copyToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem pasteToolStripMenuItem;
        public System.Windows.Forms.ToolStrip toolStrip;
        public System.Windows.Forms.ToolStripLabel positionSnapLabel;
        public System.Windows.Forms.ToolStripTextBox positionSnapTextBox;
        public System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        public System.Windows.Forms.ToolStripLabel rotationSnapLabel;
        public System.Windows.Forms.ToolStripTextBox rotationSnapTextBox;
        public System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        public System.Windows.Forms.ToolStripButton createEntityButton;
        public System.Windows.Forms.ToolStripComboBox createEntityComboBox;
        public System.Windows.Forms.ToolStripLabel creationDepthLabel;
        public System.Windows.Forms.ToolStripTextBox creationDepthTextBox;
        public System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        public System.Windows.Forms.ContextMenuStrip contextMenuStrip;
        public System.Windows.Forms.ToolStripMenuItem copyContextMenuItem;
        public System.Windows.Forms.ToolStripMenuItem pasteContextMenuItem;
        public System.Windows.Forms.ToolStripMenuItem cutToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem deleteToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem cutContextMenuItem;
        public System.Windows.Forms.ToolStripMenuItem deleteContextMenuItem;
        public System.Windows.Forms.ToolStripMenuItem newToolStripMenuItem;
        public System.Windows.Forms.ToolStripSeparator toolStripMenuItem4;
        public System.Windows.Forms.ToolStripMenuItem createToolStripMenuItem;
        public System.Windows.Forms.ToolStripButton interactButton;
        public System.Windows.Forms.ToolStripMenuItem createContextMenuItem;
        public System.Windows.Forms.ToolStripSeparator toolStripMenuItem3;
        public System.Windows.Forms.ToolStripButton resetCameraButton;
        public System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
        public System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
        public System.Windows.Forms.SplitContainer splitContainer1;
        public System.Windows.Forms.ToolStripMenuItem editToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem undoToolStripMenuItem;
        public System.Windows.Forms.ToolStripMenuItem redoToolStripMenuItem;
        public System.Windows.Forms.ToolStripButton deleteEntityButton;
        public System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
        public System.Windows.Forms.ToolStripButton undoButton;
        public System.Windows.Forms.ToolStripButton redoButton;
        public System.Windows.Forms.ToolStripSeparator toolStripSeparator6;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator7;
        public System.Windows.Forms.ToolStripButton quickSizeToolStripButton;

    }
}

