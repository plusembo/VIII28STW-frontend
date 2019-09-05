package com.ceciltechnology.viii28stw.frontend.service;

import com.ceciltechnology.viii28stw.frontend.model.dto.UserDto;
import java.util.List;

public interface IUserService {
    UserDto searchUserMaxId();
    UserDto searchUserById(String id);
    List<UserDto> searchAllUsers();
    UserDto saveUser(UserDto userDto);
    UserDto updateUser(UserDto userDto);
    boolean deleteUserById(String id);
    UserDto login(String email, String password);
    void logout(String email);
}
