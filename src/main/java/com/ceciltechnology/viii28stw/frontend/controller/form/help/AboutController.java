package com.ceciltechnology.viii28stw.frontend.controller.form.help;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Label;
import javafx.stage.Stage;
import lombok.NoArgsConstructor;
import lombok.Setter;
import java.net.URL;
import java.util.ResourceBundle;

@NoArgsConstructor
public class AboutController implements Initializable {

    @FXML
    private Label lblVersao;
    @Setter
    private Stage aboutStage;

    /**
     * Initializes the controller class.
     *
     * @param url
     * @param rb
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        lblVersao.setText("Versao");
    }

    @FXML
    private void btnCloseAction() {
        aboutStage.close();
    }

}
