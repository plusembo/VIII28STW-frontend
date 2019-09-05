package com.ceciltechnology.viii28stw.frontend.controller.form.registrations;

import com.ceciltechnology.viii28stw.frontend.enumeration.Sex;
import com.ceciltechnology.viii28stw.frontend.model.dto.UserDto;
import com.ceciltechnology.viii28stw.frontend.service.IUserService;
import com.ceciltechnology.viii28stw.frontend.util.PasswordValidator;
import com.ceciltechnology.viii28stw.frontend.util.dialogbox.DialogBoxFactory;
import com.ceciltechnology.viii28stw.frontend.model.vo.User;
import com.ceciltechnology.viii28stw.frontend.util.EmailValidator;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.net.URL;
import java.time.LocalDate;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

@NoArgsConstructor
public class CadastroUsuarioController implements Initializable {
    private final ObservableList<Sex> obsListSexo = FXCollections.observableArrayList();
    @FXML
    private TextField jmskCodigo;
    @FXML
    private TextField jtxNome;
    @FXML
    private TextField jtxSobrenome;
    @FXML
    private ComboBox<Sex> jcbxSexo;
    @FXML
    private TextField jtxEmail;
    @FXML
    private PasswordField jpwSenha;
    @FXML
    private PasswordField jpwConfirmarSenha;
    @FXML
    private Button jbtnLocalizarUsuario;
    @FXML
    private Button jbtnSalvar;
    @FXML
    private Button jbtnExcluir;
    @FXML
    private Button jbtnFechar;
    @FXML
    private Button jbtnLimpar;
    @Setter
    private boolean modoEdicao;
    @Autowired
    private IUserService userService;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        jbtnExcluir.setDisable(true);
        Sex.getList().forEach(obsListSexo::add);
        jcbxSexo.setItems(obsListSexo);
        jmskCodigo.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                                  Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
                if (true) { //jmskCodigo.validate()
                    jmskCodigoFocusLost();
                } else {
                    if (jbtnLocalizarUsuario.isFocused() || jbtnLimpar.isFocused() || jbtnFechar.isFocused()) {
//                        jmskCodigo.resetValidation();
                        return;
                    }
                    jmskCodigo.requestFocus();
                }
            }
        });

        jbtnLocalizarUsuario.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                                            Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
                if (true) { //!jmskCodigo.validate()
                    jmskCodigo.requestFocus();
                }
            }
        });


        jtxNome.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                               Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
//                jtxNome.validate();
            }
        });

        jtxSobrenome.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                                    Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
//                jtxSobrenome.validate();
            }
        });

        jcbxSexo.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                                Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
                if (jcbxSexo.getValue() == null) {
//                    lblSexoObrigatorio.setVisible(true);
//                    imgvwSexoObrigatorio.setVisible(true);
                } else {
//                    lblSexoObrigatorio.setVisible(false);
//                    imgvwSexoObrigatorio.setVisible(false);
                }
            }
        });

        jtxEmail.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                                Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
                if (true) { //jtxEmail.validate()
                    if (Boolean.TRUE.equals(EmailValidator.isValidEmail(jtxEmail.getText()))) {
//                        lblEmailInvalido.setVisible(false);
//                        imgvwEmailInvalido.setVisible(false);
                    } else {
//                        lblEmailInvalido.setText("E-mail: Inválido");
//                        lblEmailInvalido.setVisible(true);
//                        imgvwEmailInvalido.setVisible(true);
                    }
                } else {
//                    lblEmailInvalido.setVisible(false);
//                    imgvwEmailInvalido.setVisible(false);
                }
            }
        });

        jpwSenha.focusedProperty().addListener((ObservableValue<? extends Boolean> arg0,
                                                Boolean oldPropertyValue, Boolean newPropertyValue) -> {
            if (Boolean.TRUE.equals(oldPropertyValue)) {
                if (true) { //jpwSenha.validate()
                    if (Boolean.TRUE.equals(PasswordValidator.isValidPassword(jpwSenha.getText()))) {
//                        lblSenhaInvalida.setVisible(false);
//                        imgvwSenhaInvalida.setVisible(false);
                    } else {
//                        lblSenhaInvalida.setText("Senha: Inválido");
//                        lblSenhaInvalida.setVisible(true);
//                        imgvwSenhaInvalida.setVisible(true);
                    }
                } else {
//                    lblSenhaInvalida.setVisible(false);
//                    imgvwSenhaInvalida.setVisible(false);
                }
            }
        });

        jpwConfirmarSenha.focusedProperty().addListener(new ChangeListener<Boolean>() {
            @Override
            public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
                if (Boolean.TRUE.equals(oldValue)) {
                    if (jpwSenha.getText().equals(jpwConfirmarSenha.getText())) {
//                        lblConfirmarSenha.setVisible(false);
//                        imgvwConfirmarSenha.setVisible(false);
                    } else {
//                        lblConfirmarSenha.setVisible(true);
//                        imgvwConfirmarSenha.setVisible(true);
                    }
                }
            }
        });

    }

    private void jmskCodigoFocusLost() {
        if (Boolean.FALSE.equals(jmskCodigo.isEditable())) {
            return;
        }
        UserDto userDto = userService.searchUserById(jmskCodigo.getText());
        jmskCodigo.setEditable(false);
        if (userDto == null) {
            return;
        }

        User user = User.builder()
                .id(userDto.getId())
                .fullName(userDto.getFullName())
                .nickName(userDto.getNickName())
                .email(userDto.getEmail())
                .userAcessLevel(userDto.getUserAcessLevel())
                .password(userDto.getPassword())
                .sex(userDto.getSex())
                .dateOfBirth(userDto.getDateOfBirth())
                .build();

        preencheUsuario(user);

    }

    @FXML
    private void btnSearchUserAction() {

    }

    @FXML
    private void jbtnSalvarAction() {
        if (Boolean.FALSE.equals(validaTodosOsCampos())) {
            return;
        }
        UserDto userDto = UserDto.builder()
                .fullName(jtxNome.getText())
                .nickName(jtxNome.getText())
                .sex(jcbxSexo.getValue())
                .email(jtxEmail.getText())
                .password(jpwSenha.getText())
                .dateOfBirth(LocalDate.now())
                .build();

        UserDto userSalvo = userService.saveUser(userDto);
        boolean salvou = userSalvo == null;

        jbtnSalvar.setText("SALVAR");

//        JFXDialogLayout content = new JFXDialogLayout();
//        content.setHeading(new Text("Salvar Usuário"));
//        content.setBody(new Text("Usuário salvo com sucesso"));
//        content.setBody(new Text("Usuário salvo com sucesso!\n".concat(usuarioSalvo.toString())));
    }

    @FXML
    private void jbtnExcluirAction() {
        try {
            if (Boolean.FALSE.equals(DialogBoxFactory.getInstance().confirm("Excluir usuário", "Este usuário será excluido permanentemente",
                    "Tem certeza que deseja excluir este usuário ?"))) {
                return;
            }
            userService.deleteUserById(jmskCodigo.getText());
            //notificacoes.notificaExcluido();
            limpaForm();
            jmskCodigo.requestFocus();
        } catch (IOException ex) {
            Logger.getLogger(CadastroUsuarioController.class.getName()).log(Level.SEVERE, ex.getMessage(), ex);
        }
    }

    @FXML
    public void jmskCodigoUsuarioKeyReleased(KeyEvent keyEvent) {
        if (keyEvent.getCode() == KeyCode.ENTER) {
            if (true) { //jmskCodigo.validate()
                jtxNome.requestFocus();
            } else {
                jmskCodigo.requestFocus();
            }
        }
    }

    @FXML
    public void jmskCodigoUsuarioKeyPressed(KeyEvent keyEvent) {
        if (keyEvent.getCode() == KeyCode.F1) {
//            Integer csb = UsuarioService.getInstance().selecionaCodigoUsuarioSubsequente();
//            jmskCodigo.setText(csb.toString());
//            jmskCodigo.positionCaret(csb.toString().length());
        }
    }

    private void preencheUsuario(User user) {
        jbtnExcluir.setDisable(false);
        setModoEdicao(true);

        jmskCodigo.setText(user.getId());
        jtxNome.setText(user.getFullName());
        jtxNome.setText(user.getNickName());
        jcbxSexo.getSelectionModel().select(user.getSex());
        jtxEmail.setText(user.getEmail());
        jpwSenha.setText(user.getPassword());
        jpwConfirmarSenha.setText(user.getPassword());
        jbtnSalvar.setText("ATUALIZAR");
    }

    private boolean validaTodosOsCampos() {
        int campoIndex = 0;

        if (jpwSenha.getText().equals(jpwConfirmarSenha.getText())) {
//            lblConfirmarSenha.setVisible(true);
//            imgvwConfirmarSenha.setVisible(true);
            campoIndex = 6;
        }

        if (true) { //jpwSenha.validate()
            if (Boolean.FALSE.equals(PasswordValidator.isValidPassword(jpwSenha.getText()))) {
//                lblSenhaInvalida.setText("Senha: Inválido");
//                lblSenhaInvalida.setVisible(true);
//                imgvwSenhaInvalida.setVisible(true);
                campoIndex = 5;
            }
        } else {
            campoIndex = 5;
        }

        if (true) { //jtxEmail.validate()
            if (Boolean.FALSE.equals(EmailValidator.isValidEmail(jtxEmail.getText()))) {
//                lblEmailInvalido.setText("E-mail: Inválido");
//                lblEmailInvalido.setVisible(true);
//                imgvwEmailInvalido.setVisible(true);
                campoIndex = 4;
            }
        } else {
            campoIndex = 4;
        }

        if (jcbxSexo.getValue() == null) {
//            lblSexoObrigatorio.setVisible(true);
//            imgvwSexoObrigatorio.setVisible(true);
            campoIndex = 3;
        }

//        campoIndex = jtxSobrenome.validate() ? campoIndex : 2;
//        campoIndex = jtxNome.validate() ? campoIndex : 1;

        switch (campoIndex) {
            case 1:
                jtxNome.requestFocus();
                return false;
            case 2:
                jtxSobrenome.requestFocus();
                return false;
            case 3:
                jcbxSexo.requestFocus();
                return false;
            case 4:
                jtxEmail.requestFocus();
                return false;
            case 5:
                jpwSenha.requestFocus();
                return false;
            case 6:
                jpwConfirmarSenha.requestFocus();
                return false;
            default:
                return true;
        }
    }

    @FXML
    private void jbtnLimparAction() {
//        jmskCodigo.resetValidation();
//        jtxNome.resetValidation();
//        jtxSobrenome.resetValidation();
//
//        lblSexoObrigatorio.setVisible(false);
//        imgvwSexoObrigatorio.setVisible(false);
//
//        jtxEmail.resetValidation();
//        lblEmailInvalido.setVisible(false);
//        imgvwEmailInvalido.setVisible(false);
//
//        jpwSenha.resetValidation();
//        lblSenhaInvalida.setVisible(false);
//        imgvwSenhaInvalida.setVisible(false);
//
//        lblConfirmarSenha.setVisible(false);
//        imgvwConfirmarSenha.setVisible(false);
//
        limpaForm();

        jtxNome.clear();
        jtxSobrenome.clear();
        jcbxSexo.getSelectionModel().select(null);
        jtxEmail.clear();
        jpwSenha.clear();
        jpwConfirmarSenha.clear();

        jmskCodigo.requestFocus();

    }

    private void limpaForm() {
        jmskCodigo.setEditable(true);
        jbtnExcluir.setDisable(true);
        setModoEdicao(false);

        jmskCodigo.setText("");
        jtxNome.clear();
        jtxSobrenome.clear();
        jcbxSexo.getSelectionModel().select(null);
        jtxEmail.clear();
        jpwSenha.clear();
        jpwConfirmarSenha.clear();

        jbtnSalvar.setText("SALVAR");
    }
}
