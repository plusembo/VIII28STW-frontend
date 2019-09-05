package com.ceciltechnology.viii28stw.frontend.model.domain;

import com.ceciltechnology.viii28stw.frontend.model.vo.User;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.stereotype.Component;

/**
 * @author Plamedi L. Lusembo
 */

@NoArgsConstructor
@Component
@Getter
@Setter
public class Session {
    private User user;
    private boolean logoutRequest;
}