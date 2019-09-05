package com.ceciltechnology.viii28stw.frontend.controller;

import com.ceciltechnology.viii28stw.frontend.MainApp;
import com.ceciltechnology.viii28stw.frontend.enumeration.Menus;
import com.ceciltechnology.viii28stw.frontend.model.domain.Session;
import com.ceciltechnology.viii28stw.frontend.util.I18nFactory;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import javafx.util.Duration;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.io.IOException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Plamedi L. Lusembo
 */

@NoArgsConstructor
@Component
public class MainController implements Initializable {
    private static HashMap<Menus, Tab> listForms;
    @Setter
    private Stage mainStage;
    @FXML
    private TabPane tbpMainTabPane;
    @FXML
    private Hyperlink hlkNomeUsuario;
    @FXML
    private Label lblDataHora;

    @Autowired
    Session session;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        lblDataHora.setText("");
        listForms = new HashMap<>();
        KeyFrame frame = new KeyFrame(Duration.millis(1000), e -> updateDateTime());
        Timeline timeline = new Timeline(frame);
        timeline.setCycleCount(Timeline.INDEFINITE);
        timeline.play();

        hlkNomeUsuario.setText(session.getUser().getFullName()
                .concat(" (")
                .concat(session.getUser().getEmail())
                .concat(")"));
    }

    private void openTab(Menus menus) {
        try {
            if (Boolean.TRUE.equals(listForms.containsKey(menus))) {
                tbpMainTabPane.getSelectionModel().select(listForms.get(menus));
            } else {
                FXMLLoader loader = new FXMLLoader();
                loader.setResources(I18nFactory.getInstance().getResourceBundle());
                loader.setLocation(MainApp.class.getResource(menus.getFxmlPath()));
                BorderPane parent = loader.load();
                if (null != menus.getIcon() && !menus.getIcon().isEmpty()) {
                    //formStage.getIcons().add(new Image(menuMDI.getIcon()));
                }
                Tab tab = new Tab();
                Label label = new Label("Nova tab");
                tab.setGraphic(label);
                tab.setContent(parent);
                tbpMainTabPane.getTabs().add(tab);
                tbpMainTabPane.getSelectionModel().select(tab);
                tab.setOnCloseRequest((Event we) -> closeTab(menus));
                listForms.put(menus, tab);
            }

        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, ex.getMessage(), ex);
        }
    }

    private static void closeTab(Menus menumenuMDI) {
        try {
            listForms.remove(menumenuMDI);
        } catch (NullPointerException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, ex.getMessage(), ex);
        }
    }

    private void updateDateTime() {
        DateFormat df = DateFormat.getDateInstance(DateFormat.FULL, I18nFactory.getInstance().getLocale());
        String data = df.format(Calendar.getInstance().getTime());
        String hora = new SimpleDateFormat("HH:mm:ss").format(Calendar.getInstance().getTime());
        lblDataHora.setText(data.substring(0, 1).toUpperCase().concat(data.substring(1)).concat(" ").concat(hora));
    }

    @FXML
    private void hlkSairOnAction() {
        mainStage.close();
    }

    @FXML
    private void mnuConfiguracaoContaUsuarioAction() {
        openTab(Menus.MY_ACCOUNT_SETTING);
    }

    @FXML
    private void mnuSetUpSystemLanguageAction() {
        openTab(Menus.LANGUAGE_SETTING);
    }

    @FXML
    private void mnuCadastroTipoRendaAction() {
        openTab(Menus.INCOME_TYPE_REGISTRATION);
    }

    @FXML
    private void mnuCadastroRendaAction() {
        openTab(Menus.INCOME_REGISTRATION);
    }

    @FXML
    private void mnuCadastroTipoDespesaAction() {
        openTab(Menus.EXPENSE_TYPE_REGISTRATION);
    }

    @FXML
    private void mnuCadastroDespesaAction() {
        openTab(Menus.EXPENSE_REGISTRATION);
    }

    @FXML
    private void mnuCadastroUsuarioAction() {
        openTab(Menus.USER_REGISTRATION);
    }

    @FXML
    private void mnuRelatorioRendaAction() {
        openTab(Menus.INCOME_REPORT);
    }

    @FXML
    private void mnuRelatorioDespesasAction() {
        openTab(Menus.EXPENSE_REPORT);
    }

    @FXML
    private void mnuAjudaSobreAction() {
        openTab(Menus.ABOUT);
    }

}
