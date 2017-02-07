using System;
using System.Drawing;
using System.Linq;
using ScintillaNET;
using ScintillaNET_FindReplaceDialog;
using System.Windows.Forms;

namespace Nu.Gaia.Design
{
    public class SymbolicTextBox : Scintilla
    {
        public SymbolicTextBox()
        {
            // Make default styles monospaced!
            Styles[Style.Default].Font = "Lucida Console";

            // Add a little more line spacing for new font
            ExtraDescent = 1;

            // Lisp lexer
            Lexer = Lexer.Lisp;

            // Add comment styles. NOTE: this currently doesn't work.
            Styles[Style.Lisp.Comment].ForeColor = Color.ForestGreen;
            Styles[Style.Lisp.MultiComment].ForeColor = Color.ForestGreen;
            SetKeywords(2, "// (* *)");

            // Add keyword styles (keywords 0 are reserved for DSL-specific use)
            Styles[Style.Lisp.Keyword].ForeColor = Color.DarkBlue;
            Styles[Style.Lisp.KeywordKw].ForeColor = Color.FromArgb(0xFF, 0x60, 0x00, 0x70);
            SetKeywords(1, keywordsImplicit);

            // Add operator styles (braces, actually)
            Styles[Style.Lisp.Operator].ForeColor = Color.RoyalBlue;
            Styles[Style.BraceLight].BackColor = Color.LightBlue;
            Styles[Style.BraceBad].BackColor = Color.Red;

            // Add symbol styles (operators, actually)
            Styles[Style.Lisp.Special].ForeColor = Color.DarkBlue;

            // Add string style
            Styles[Style.Lisp.String].ForeColor = Color.Teal;

            // No tabs
            UseTabs = false;

            // Implement brace matching
            UpdateUI += SymbolicTextBox_UpdateUI;

            // Implement auto-complete
            CharAdded += SymbolicTextBox_CharAdded;

            // Implement find/replace
            MyFindReplace = new FindReplace(this);
            KeyDown += SymbolicTextBox_KeyDown;
        }

        public string Keywords0
        {
            get { return keywords0; }
            set
            {
                keywords0 = value;
                SetKeywords(0, keywords0);
            }
        }

        public string Keywords1
        {
            get { return keywords1; }
            set
            {
                keywords1 = value;
                SetKeywords(1, keywords1 + " " + keywordsImplicit);
            }
        }

        public string KeywordsImplicit
        {
            get { return keywordsImplicit; }
            set
            {
                keywordsImplicit = value;
                SetKeywords(1, keywords1 + " " + keywordsImplicit);
            }
        }

        public string AutoCWords
        {
            get
            {
                var keywordsSplit = keywords0.Split(' ').Distinct().ToArray();
                Array.Sort(keywordsSplit);
                var keywordsSorted = string.Join(AutoCSeparator.ToString(), keywordsSplit);
                return keywordsSorted;
            }
        }

        private void SymbolicTextBox_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control && !e.Shift && e.KeyCode == Keys.F)
            {
                MyFindReplace.ShowIncrementalSearch();
                e.SuppressKeyPress = true;
            }
            else if (e.Control && !e.Shift && e.KeyCode == Keys.H)
            {
                MyFindReplace.ShowReplace();
                e.SuppressKeyPress = true;
            }
            else if (e.Control && e.Shift && e.KeyCode == Keys.F)
            {
                MyFindReplace.ShowFind();
                e.SuppressKeyPress = true;
            }
            else if (e.Control && e.Shift && e.KeyCode == Keys.H)
            {
                MyFindReplace.ShowReplace();
                e.SuppressKeyPress = true;
            }
            else if (e.Control && e.KeyCode == Keys.G)
            {
                GoTo MyGoTo = new GoTo(this);
                MyGoTo.ShowGoToDialog();
                e.SuppressKeyPress = true;
            }
            else if (e.Control && e.KeyCode == Keys.F3)
            {
                // TODO: figure out how to call this from here...
                // https://github.com/Stumpii/ScintillaNET-FindReplaceDialog/issues
                // MyFindReplace.FindNext();
                // e.SuppressKeyPress = true;
            }
            else if (e.Control && e.KeyCode == Keys.Escape)
            {
                MyFindReplace.ClearAllHighlights();
                e.SuppressKeyPress = true;
            }
            else if (e.Control && e.KeyCode == Keys.Space)
            {
                AutoCShow(false);
                e.SuppressKeyPress = true;
            }
            else if (e.Alt && e.KeyCode == Keys.Up)
            {
                // TODO: SelectParentSymbols();
                e.SuppressKeyPress = true;
            }
            else if (e.Alt && e.KeyCode == Keys.Down)
            {
                // TODO: SelectChildSymbols();
                e.SuppressKeyPress = true;
            }
        }

        private void SymbolicTextBox_CharAdded(object sender, CharAddedEventArgs e)
        {
            AutoCShow(true);
        }

        private void SymbolicTextBox_UpdateUI(object sender, UpdateUIEventArgs e)
        {
            // Has the selection changed position?
            var selectionPos = SelectionStart;
            if (lastSelectionPos != selectionPos)
            {
                lastSelectionPos = selectionPos;
                var bracePos1 = -1;
                var bracePos2 = -1;
                if (IsBraceLeft(GetCharAt(selectionPos)))
                {
                    // Select the brace to the immediate right
                    bracePos1 = selectionPos;
                }
                else if (selectionPos > 0 && IsBraceRight(GetCharAt(selectionPos - 1)))
                {
                    // Select the brace to the immediate left
                    bracePos1 = selectionPos - 1;
                }

                if (bracePos1 >= 0)
                {
                    // Find the matching brace
                    bracePos2 = BraceMatch(bracePos1);
                    if (bracePos2 == InvalidPosition)
                    {
                        BraceBadLight(bracePos1);
                        HighlightGuide = 0;
                    }
                    else
                    {
                        BraceHighlight(bracePos1, bracePos2);
                        HighlightGuide = GetColumn(bracePos1);
                    }
                }
                else
                {
                    // Turn off brace matching
                    BraceHighlight(InvalidPosition, InvalidPosition);
                    HighlightGuide = 0;
                }
            }
        }

        private void AutoCShow(bool requireTextInCurrentWord)
        {
            // Find the word start
            var currentPos = CurrentPosition;
            var wordStartPos = WordStartPosition(currentPos, true);

            // Display the autocompletion list
            var lenEntered = currentPos - wordStartPos;
            if (!requireTextInCurrentWord || lenEntered > 0) AutoCShow(lenEntered, AutoCWords);
        }

        private bool IsBraceLeft(int c)
        {
            return c == '[';
        }

        private bool IsBraceRight(int c)
        {
            return c == ']';
        }

        private string keywords0 = string.Empty;
        private string keywords1 = string.Empty;
        private string keywordsImplicit = "True False Some None Right Left";
        private int lastSelectionPos = 0;
        private FindReplace MyFindReplace;
    }
}
