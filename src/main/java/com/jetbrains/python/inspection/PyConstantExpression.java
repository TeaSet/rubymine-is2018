package com.jetbrains.python.inspection;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.*;
import com.jetbrains.python.inspections.PyInspection;
import com.jetbrains.python.inspections.PyInspectionVisitor;
import com.jetbrains.python.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;

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

        private void processIfPart(@NotNull PyIfPart pyIfPart) {
            final PyExpression condition = pyIfPart.getCondition();
            Objects.requireNonNull(condition);

            if (condition instanceof PyBoolLiteralExpression) {
                registerProblem(condition, "The condition is always " + ((PyBoolLiteralExpression) condition).getValue());
            } else if (condition instanceof PyBinaryExpression) {

                // >, <, ==, !=
                Optional<PsiElement> opSign = Optional.ofNullable(((PyBinaryExpression) condition).getPsiOperator());
                String operation;


                //left_number -> int
                PsiElement leftExpr = condition.getFirstChild();
                int left = Integer.parseInt(leftExpr.getText());

                //right_number -> int
                PsiElement rightExpr = condition.getLastChild();
                int right = Integer.parseInt(rightExpr.getText());


                if (opSign.isPresent()) {
                    operation = opSign.get().getText();
                } else {
                    throw new NullPointerException("The operational sign was not be set!");
                }

                registerProblem(condition, "The condition is always "
                                                        + parseExpression(left, operation, right));
            }
        }

        private boolean parseExpression(int left, String operation, int right) {
            switch (operation) {
                //Use these types of conditional sight according to the task's rules
                case ">":
                    return left > right;
                case "<":
                    return left < right;
                case "==":
                    return left == right;
                case "!=":
                    return left != right;
            }
            throw new IllegalArgumentException("The conditional expression is incorrect!");
        }
    }
}
