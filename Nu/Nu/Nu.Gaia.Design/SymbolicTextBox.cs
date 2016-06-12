using System.Drawing;
using ScintillaNET;

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

            // Add keyword styles
            Styles[Style.Lisp.Keyword].ForeColor = Color.DarkBlue;
            Styles[Style.Lisp.KeywordKw].ForeColor = Color.FromArgb(0xFF, 0x60, 0x00, 0x70);
            SetKeywords(1, "True False Some None Left Right");

            // Add special character styles
            Styles[Style.Lisp.Operator].ForeColor = Color.RoyalBlue; // brackets, actually

            // Add string style
            Styles[Style.Lisp.String].ForeColor = Color.Teal;

            // Add brace matching styles
            Styles[Style.BraceLight].BackColor = Color.LightGray;
            Styles[Style.BraceBad].BackColor = Color.Red;

            // No tabs
            UseTabs = false;

            // Implement brace matching
            UpdateUI += SymbolicTextBox_UpdateUI;
        }

        private void SymbolicTextBox_UpdateUI(object sender, UpdateUIEventArgs e)
        {
            // Has the caret changed position?
            var caretPos = CurrentPosition;
            if (lastCaretPos != caretPos)
            {
                lastCaretPos = caretPos;
                var bracePos1 = -1;
                var bracePos2 = -1;

                if (IsBrace(GetCharAt(caretPos)))
                {
                    // Select the brace to the immediate right
                    bracePos1 = caretPos;
                }
                else
                {
                    // Select the brace anywhere to the left
                    for (var i = caretPos - 1; i >= 0; --i)
                    {
                        if (IsBrace(GetCharAt(i)))
                        {
                            bracePos1 = i;
                            break;
                        }
                    }
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

        private bool IsBrace(int c)
        {
            switch (c)
            {
                case '(':
                case ')':
                case '[':
                case ']':
                case '{':
                case '}': return true;
            }
            return false;
        }

        private int lastCaretPos = 0;
    }
}
