package com.ceciltechnology.viii28stw.frontend.model.vo;

import com.ceciltechnology.viii28stw.frontend.enumeration.Sex;
import com.ceciltechnology.viii28stw.frontend.enumeration.UserAcessLevel;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDate;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@ToString
@Builder
public class User implements Serializable {
    private static final long serialVersionUID = 1L;
    private String id;
    private String fullName;
    private String nickName;
    private String email;
    private String password;
    private Sex sex;
    private LocalDate dateOfBirth;
    private UserAcessLevel userAcessLevel;
}
