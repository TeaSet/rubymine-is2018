package com.jetbrains.python.inspection;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.*;
import com.jetbrains.python.PyTokenTypes;
import com.jetbrains.python.inspections.PyInspection;
import com.jetbrains.python.inspections.PyInspectionVisitor;
import com.jetbrains.python.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class PyConstantExpression extends PyInspection {

    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly,
                                          @NotNull LocalInspectionToolSession session) {
        return new Visitor(holder, session);
    }

    private static class Visitor extends PyInspectionVisitor {

        private Visitor(@Nullable ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
            super(holder, session);
        }

        @Override
        public void visitPyIfStatement(PyIfStatement node) {
            super.visitPyIfStatement(node);
            processIfPart(node.getIfPart());
            for (PyIfPart part : node.getElifParts()) {
                processIfPart(part);
            }
        }

        private boolean processExpression(PsiElement element) {
            boolean res = false;

            if (element instanceof PyBinaryExpression) {
                PyElementType operator = ((PyBinaryExpression) element).getOperator();

                PsiElement leftSide = element.getFirstChild();
                int left_num = Integer.parseInt(leftSide.getText());

                PsiElement rightSide = element.getLastChild();
                int right_num = Integer.parseInt(rightSide.getText());

                if (PyTokenTypes.GT.equals(operator)) {
                    res = left_num > right_num;
                }
                if (PyTokenTypes.LT.equals(operator)) {
                    res = left_num < right_num;
                }
                if (PyTokenTypes.EQEQ.equals(operator)) {
                    res = left_num == right_num;
                }
                if (PyTokenTypes.NE.equals(operator)) {
                    res = left_num != right_num;
                }
            } else if (element instanceof PyPrefixExpression)  {
                res = processExpression(element.getFirstChild().getNextSibling());

                res = res ? false : true;

            } else if (element instanceof PyParenthesizedExpression) {
                res = processExpression(element.getFirstChild().getNextSibling());
            }
            return res;
        }

        private void processIfPart(@NotNull PyIfPart pyIfPart) {
            final PyExpression condition = pyIfPart.getCondition();
            Objects.requireNonNull(condition);

            if (condition instanceof PyBoolLiteralExpression) {
                registerProblem(condition, "The condition is always " + ((PyBoolLiteralExpression) condition).getValue());
            } else {
                boolean res = false;
                //Simple way: when first and last child are already PyNumericLiteralExpression
                if (condition.getFirstChild() instanceof PyNumericLiteralExpression ||
                        condition.getLastChild() instanceof PyNumericLiteralExpression) {
                    res = processExpression(condition);
                } else {
                    //Apply specific 'divide et impera' method
                    boolean left_res = processExpression(condition.getFirstChild());
                    boolean right_res = processExpression(condition.getLastChild());
                    PyElementType operator = ((PyBinaryExpression) condition).getOperator();
                    if (PyTokenTypes.AND_KEYWORD.equals(operator)) {
                        res = left_res && right_res;
                    }
                    if (PyTokenTypes.OR_KEYWORD.equals(operator)) {
                        res = left_res || right_res;
                    }
                }
                registerProblem(condition, "The condition is always " + res);
            }
        }
    }
}

/*
if 3 > 4:
    pass

PyFile:Dummy.py(0,18)
  PyIfStatement(0,18)
    PyIfPartIf(0,18)
      PsiElement(Py:IF_KEYWORD)('if')(0,2)
      PsiWhiteSpace(' ')(2,3)
      PyBinaryExpression(3,8)
        PyNumericLiteralExpression(3,4)
          PsiElement(Py:INTEGER_LITERAL)('3')(3,4)
        PsiWhiteSpace(' ')(4,5)
        PsiElement(Py:GT)('>')(5,6)
        PsiWhiteSpace(' ')(6,7)
        PyNumericLiteralExpression(7,8)
          PsiElement(Py:INTEGER_LITERAL)('4')(7,8)
      PsiElement(Py:COLON)(':')(8,9)
      PsiWhiteSpace('\n    ')(9,14)
      PyStatementList(14,18)
        PyPassStatement(14,18)
          PsiElement(Py:PASS_KEYWORD)('pass')(14,18)

*/

/*
if 3 > 4 and 5 < 10:
    pass

    PyFile:Dummy.py(0,29)
  PyIfStatement(0,29)
    PyIfPartIf(0,29)
      PsiElement(Py:IF_KEYWORD)('if')(0,2)
      PsiWhiteSpace(' ')(2,3)
      PyBinaryExpression(3,19)
        PyBinaryExpression(3,8)
          PyNumericLiteralExpression(3,4)
            PsiElement(Py:INTEGER_LITERAL)('3')(3,4)
          PsiWhiteSpace(' ')(4,5)
          PsiElement(Py:GT)('>')(5,6)
          PsiWhiteSpace(' ')(6,7)
          PyNumericLiteralExpression(7,8)
            PsiElement(Py:INTEGER_LITERAL)('4')(7,8)
        PsiWhiteSpace(' ')(8,9)
        PsiElement(Py:AND_KEYWORD)('and')(9,12)
        PsiWhiteSpace(' ')(12,13)
        PyBinaryExpression(13,19)
          PyNumericLiteralExpression(13,14)
            PsiElement(Py:INTEGER_LITERAL)('5')(13,14)
          PsiWhiteSpace(' ')(14,15)
          PsiElement(Py:LT)('<')(15,16)
          PsiWhiteSpace(' ')(16,17)
          PyNumericLiteralExpression(17,19)
            PsiElement(Py:INTEGER_LITERAL)('10')(17,19)
      PsiElement(Py:COLON)(':')(19,20)
      PsiWhiteSpace('\n    ')(20,25)
      PyStatementList(25,29)
        PyPassStatement(25,29)
          PsiElement(Py:PASS_KEYWORD)('pass')(25,29)

 */

/*
if not(3 > 4) or 5 > 10:
    pass

PyFile:Dummy.py(0,33)
  PyIfStatement(0,33)
    PyIfPartIf(0,33)
      PsiElement(Py:IF_KEYWORD)('if')(0,2)
      PsiWhiteSpace(' ')(2,3)
      PyBinaryExpression(3,23)
        PyPrefixExpression(3,13)
          PsiElement(Py:NOT_KEYWORD)('not')(3,6)
          PyParenthesizedExpression(6,13)
            PsiElement(Py:LPAR)('(')(6,7)
            PyBinaryExpression(7,12)
              PyNumericLiteralExpression(7,8)
                PsiElement(Py:INTEGER_LITERAL)('3')(7,8)
              PsiWhiteSpace(' ')(8,9)
              PsiElement(Py:GT)('>')(9,10)
              PsiWhiteSpace(' ')(10,11)
              PyNumericLiteralExpression(11,12)
                PsiElement(Py:INTEGER_LITERAL)('4')(11,12)
            PsiElement(Py:RPAR)(')')(12,13)
        PsiWhiteSpace(' ')(13,14)
        PsiElement(Py:OR_KEYWORD)('or')(14,16)
        PsiWhiteSpace(' ')(16,17)
        PyBinaryExpression(17,23)
          PyNumericLiteralExpression(17,18)
            PsiElement(Py:INTEGER_LITERAL)('5')(17,18)
          PsiWhiteSpace(' ')(18,19)
          PsiElement(Py:GT)('>')(19,20)
          PsiWhiteSpace(' ')(20,21)
          PyNumericLiteralExpression(21,23)
            PsiElement(Py:INTEGER_LITERAL)('10')(21,23)
      PsiElement(Py:COLON)(':')(23,24)
      PsiWhiteSpace('\n    ')(24,29)
      PyStatementList(29,33)
        PyPassStatement(29,33)
          PsiElement(Py:PASS_KEYWORD)('pass')(29,33)

 */