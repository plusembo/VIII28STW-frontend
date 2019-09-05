package com.ceciltechnology.viii28stw.frontend.service;

import com.ceciltechnology.viii28stw.frontend.model.domain.Session;
import com.ceciltechnology.viii28stw.frontend.model.dto.UserDto;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ceciltechnology.viii28stw.frontend.model.vo.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import java.io.IOException;
import java.util.List;

@Component
public class UserService implements IUserService {
    @Value("${basic.auth.user}")
    private String basicAuthUser;
    @Value("${basic.auth.password}")
    private String basicAuthPassword;
    @Value("${url.prefix}")
    private String urlPrefix;
    @Value("${url.login}")
    private String urlLogin;

    @Autowired
    Session session;
    @Autowired
    private RestTemplateBuilder restTemplateBuilder;
    @Autowired
    private ObjectMapper mapper;
    @Autowired
    private HttpHeaders httpHeaders;

    public UserDto searchUserMaxId() {
        return null;
    }

    @Override
    public UserDto searchUserById(String id) {
        return null;
    }

    @Override
    public List<UserDto> searchAllUsers() {
        return null;
    }

    @Override
    public UserDto saveUser(UserDto userDto) {
        return null;
    }

    @Override
    public UserDto updateUser(UserDto userDto) {
        return null;
    }

    private UserDto persist(UserDto userDto) {
        return null;
    }

    @Override
    public boolean deleteUserById(String id) {
        return true;
    }

    @Override
    public UserDto login(String email, String password) {
        UserDto userDto = UserDto.builder()
                .email(email)
                .password(password)
                .build();
        RestTemplate restTemplate = restTemplateBuilder.basicAuthentication(basicAuthUser, basicAuthPassword).build();

        ResponseEntity responseEntityUsuario = restTemplate
                .exchange(urlPrefix.concat(urlLogin), HttpMethod.POST,
                        new HttpEntity<>(userDto, httpHeaders), String.class);
        try {
            if (responseEntityUsuario.getBody() != null) {
                UserDto usuarioDto1 = mapper.readValue(responseEntityUsuario.getBody().toString(), UserDto.class);
                User usuario = User.builder()
                        .id(usuarioDto1.getId())
                        .fullName(usuarioDto1.getFullName())
                        .nickName(usuarioDto1.getNickName())
                        .email(usuarioDto1.getEmail())
                        .password(usuarioDto1.getPassword())
                        .sex(usuarioDto1.getSex())
                        .dateOfBirth(usuarioDto1.getDateOfBirth())
                        .build();
                session.setUser(usuario);
                session.setLogoutRequest(false);
                return userDto;
            }
        } catch (IOException e) {
        }
        return null;
    }

    @Override
    public void logout(String email) {
    }

}
