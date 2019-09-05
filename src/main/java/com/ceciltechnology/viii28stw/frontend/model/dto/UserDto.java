package com.ceciltechnology.viii28stw.frontend.model.dto;

import com.ceciltechnology.viii28stw.frontend.enumeration.Sex;
import com.ceciltechnology.viii28stw.frontend.enumeration.UserAcessLevel;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import lombok.*;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import java.time.LocalDate;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@Builder
public class UserDto {
    private String id;
    @NotBlank private String fullName;
    private String nickName;
    @NotBlank @Email private String email;
    @NotBlank private String password;
    private Sex sex;
    @JsonSerialize(using = LocalDateSerializer.class)
    @JsonDeserialize(using = LocalDateDeserializer.class)
    private LocalDate dateOfBirth;
    private UserAcessLevel userAcessLevel;
}
