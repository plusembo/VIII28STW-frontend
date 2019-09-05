package com.ceciltechnology.viii28stw.frontend.util.dialogbox;

import com.ceciltechnology.viii28stw.frontend.enumeration.DialogType;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.stage.Stage;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.net.URL;
import java.util.ResourceBundle;

@NoArgsConstructor
public class DialogBoxController implements Initializable {

    @Setter
    private Stage dialogBoxStage;
    @Setter
    private DialogType dialogType;
    @FXML
    @Getter
    private Label lblHeaderText;
    @FXML
    @Getter
    private Label lblContentText;
    @FXML
    private Button jbtnOkay;
    @FXML
    private Button jbtnCancel;
    @Getter
    private boolean resultOkay;
    private static DialogBoxController uniqueInstance;

    /**
     * Initializes the controller class.
     *
     * @param url
     * @param rb
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        Platform.runLater(() -> {
            if (dialogType != DialogType.CONFIRMATION) {
                jbtnCancel.setVisible(false);
                jbtnOkay.setLayoutX(jbtnCancel.getLayoutX());
                jbtnOkay.setLayoutY(jbtnCancel.getLayoutY());
            }
        });
    }

    public static synchronized DialogBoxController getInstance() {
        if (uniqueInstance == null) {
            uniqueInstance = new DialogBoxController();
        }
        return uniqueInstance;
    }

    @FXML
    private void jbtnOkayAction() {
        resultOkay = true;
        dialogBoxStage.close();
    }

    @FXML
    private void jbtnCancelAction() {
        resultOkay = false;
        dialogBoxStage.close();
    }

}