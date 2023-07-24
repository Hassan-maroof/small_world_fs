package com.smallworldfs.exception;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ServiceException extends RuntimeException {

  private final String code;
  private final String message;

  public ServiceException(String code, String message) {
    super(message);
    this.code = code;
    this.message = message;
  }

  public ServiceException(Error error, Exception e) {
    super(error.getMessage(), e);
    this.code = error.getCode();
    this.message = error.getMessage();
  }
}
