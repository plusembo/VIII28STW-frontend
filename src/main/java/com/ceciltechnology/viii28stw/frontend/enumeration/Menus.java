package com.ceciltechnology.viii28stw.frontend.enumeration;

import com.ceciltechnology.viii28stw.frontend.util.I18nFactory;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Plamedi L. Lusembo
 */
@AllArgsConstructor
@Getter
public enum Menus {
    /**
     * 0. Settings
     */
    MY_ACCOUNT_SETTING(I18nFactory.getInstance().getResourceBundle().getString("title.form.my.account.setting"), "", ""),
    LANGUAGE_SETTING(I18nFactory.getInstance().getResourceBundle().getString("text.menuitem.language.setting"), "/fxml/form/cofiguracoes/configuracao_idioma.fxml", ""),

    /**
     * 1. Registrations
     */
    INCOME_TYPE_REGISTRATION(I18nFactory.getInstance().getResourceBundle().getString("title.form.income.type.registration"), "", ""),
    INCOME_REGISTRATION(I18nFactory.getInstance().getResourceBundle().getString("title.form.income.registration"), "", ""),
    EXPENSE_TYPE_REGISTRATION(I18nFactory.getInstance().getResourceBundle().getString("title.form.expense.type.registration"), "", ""),
    EXPENSE_REGISTRATION(I18nFactory.getInstance().getResourceBundle().getString("title.form.expense.registration"), "/fxml/form/registrations/cadastro_teste.fxml", ""),
    USER_REGISTRATION(I18nFactory.getInstance().getResourceBundle().getString("title.form.user.registration"), "/fxml/form/cadastros/cadastro_usuario.fxml", ""),

    /**
     * 3. Reports
     */
    INCOME_REPORT(I18nFactory.getInstance().getResourceBundle().getString("title.form.income.report"), "", ""),
    EXPENSE_REPORT(I18nFactory.getInstance().getResourceBundle().getString("title.form.expense.report"), "", ""),

    /**
     * 4. Help
     */
    ABOUT(I18nFactory.getInstance().getResourceBundle().getString("text.menuitem.about"), "/fxml/form/ajuda/sobre.fxml", "");

    private final String title;
    private final String fxmlPath;
    private final String icon;
}