package com.service.productcatalogue.dto;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDate;
import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

class ErrorResponseDtoTest {
    /**
     * Method under test: {@link ErrorResponseDto#canEqual(Object)}
     */
    @Test
    void testCanEqual() {
        //   Diffblue Cover was unable to write a Spring test,
        //   so wrote a non-Spring test instead.
        //   Reason: R026 Failed to create Spring context.
        //   Attempt to initialize test context failed with
        //   java.lang.IllegalStateException: ApplicationContext failure threshold (1) exceeded: skipping repeated attempt to load context for [MergedContextConfiguration@379d0e52 testClass = com.service.productcatalogue.dto.DiffblueFakeClass2, locations = [], classes = [com.service.productcatalogue.dto.ErrorResponseDto, java.lang.String, org.springframework.http.HttpStatus, java.time.LocalDateTime, org.springframework.http.HttpStatus.Series], contextInitializerClasses = [], activeProfiles = [], propertySourceDescriptors = [], propertySourceProperties = [], contextCustomizers = [org.springframework.boot.test.context.filter.ExcludeFilterContextCustomizer@451482b, org.springframework.boot.test.json.DuplicateJsonObjectContextCustomizerFactory$DuplicateJsonObjectContextCustomizer@7786c760, org.springframework.boot.test.mock.mockito.MockitoContextCustomizer@0, org.springframework.boot.test.autoconfigure.actuate.observability.ObservabilityContextCustomizerFactory$DisableObservabilityContextCustomizer@1f, org.springframework.boot.test.autoconfigure.properties.PropertyMappingContextCustomizer@0, org.springframework.boot.test.autoconfigure.web.servlet.WebDriverContextCustomizer@79db549a], contextLoader = org.springframework.test.context.support.DelegatingSmartContextLoader, parent = null]
        //       at org.springframework.test.context.cache.DefaultCacheAwareContextLoaderDelegate.loadContext(DefaultCacheAwareContextLoaderDelegate.java:145)
        //       at org.springframework.test.context.support.DefaultTestContext.getApplicationContext(DefaultTestContext.java:130)
        //       at java.base/java.util.Optional.map(Optional.java:260)
        //   See https://diff.blue/R026 to resolve this issue.

        assertFalse((new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay())).canEqual("Other"));
    }

    /**
     * Method under test: {@link ErrorResponseDto#canEqual(Object)}
     */
    @Test
    void testCanEqual2() {
        //   Diffblue Cover was unable to write a Spring test,
        //   so wrote a non-Spring test instead.
        //   Reason: R026 Failed to create Spring context.
        //   Attempt to initialize test context failed with
        //   java.lang.IllegalStateException: ApplicationContext failure threshold (1) exceeded: skipping repeated attempt to load context for [MergedContextConfiguration@379d0e52 testClass = com.service.productcatalogue.dto.DiffblueFakeClass2, locations = [], classes = [com.service.productcatalogue.dto.ErrorResponseDto, java.lang.String, org.springframework.http.HttpStatus, java.time.LocalDateTime, org.springframework.http.HttpStatus.Series], contextInitializerClasses = [], activeProfiles = [], propertySourceDescriptors = [], propertySourceProperties = [], contextCustomizers = [org.springframework.boot.test.context.filter.ExcludeFilterContextCustomizer@451482b, org.springframework.boot.test.json.DuplicateJsonObjectContextCustomizerFactory$DuplicateJsonObjectContextCustomizer@7786c760, org.springframework.boot.test.mock.mockito.MockitoContextCustomizer@0, org.springframework.boot.test.autoconfigure.actuate.observability.ObservabilityContextCustomizerFactory$DisableObservabilityContextCustomizer@1f, org.springframework.boot.test.autoconfigure.properties.PropertyMappingContextCustomizer@0, org.springframework.boot.test.autoconfigure.web.servlet.WebDriverContextCustomizer@79db549a], contextLoader = org.springframework.test.context.support.DelegatingSmartContextLoader, parent = null]
        //       at org.springframework.test.context.cache.DefaultCacheAwareContextLoaderDelegate.loadContext(DefaultCacheAwareContextLoaderDelegate.java:145)
        //       at org.springframework.test.context.support.DefaultTestContext.getApplicationContext(DefaultTestContext.java:130)
        //       at java.base/java.util.Optional.map(Optional.java:260)
        //   See https://diff.blue/R026 to resolve this issue.

        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        assertTrue(errorResponseDto.canEqual(new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay())));
    }

    /**
     * Methods under test:
     *
     * <ul>
     *   <li>
     * {@link ErrorResponseDto#ErrorResponseDto(String, HttpStatus, String, LocalDateTime)}
     *   <li>{@link ErrorResponseDto#setApiPath(String)}
     *   <li>{@link ErrorResponseDto#setErrorCode(HttpStatus)}
     *   <li>{@link ErrorResponseDto#setErrorMessage(String)}
     *   <li>{@link ErrorResponseDto#setErrorTime(LocalDateTime)}
     *   <li>{@link ErrorResponseDto#toString()}
     *   <li>{@link ErrorResponseDto#getApiPath()}
     *   <li>{@link ErrorResponseDto#getErrorCode()}
     *   <li>{@link ErrorResponseDto#getErrorMessage()}
     *   <li>{@link ErrorResponseDto#getErrorTime()}
     * </ul>
     */
    @Test
    void testConstructor() {
        ErrorResponseDto actualErrorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        actualErrorResponseDto.setApiPath("Api Path");
        actualErrorResponseDto.setErrorCode(HttpStatus.CONTINUE);
        actualErrorResponseDto.setErrorMessage("An error occurred");
        LocalDateTime errorTime = LocalDate.of(1970, 1, 1).atStartOfDay();
        actualErrorResponseDto.setErrorTime(errorTime);
        String actualToStringResult = actualErrorResponseDto.toString();
        String actualApiPath = actualErrorResponseDto.getApiPath();
        HttpStatus actualErrorCode = actualErrorResponseDto.getErrorCode();
        String actualErrorMessage = actualErrorResponseDto.getErrorMessage();
        assertEquals("An error occurred", actualErrorMessage);
        assertEquals("Api Path", actualApiPath);
        assertEquals("ErrorResponseDto(apiPath=Api Path, errorCode=100 CONTINUE, errorMessage=An error occurred, errorTime"
                + "=1970-01-01T00:00)", actualToStringResult);
        assertEquals(HttpStatus.CONTINUE, actualErrorCode);
        assertSame(errorTime, actualErrorResponseDto.getErrorTime());
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals() {
        assertNotEquals(new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()), null);
        assertNotEquals(new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()), "Different type to ErrorResponseDto");
    }

    /**
     * Methods under test:
     *
     * <ul>
     *   <li>{@link ErrorResponseDto#equals(Object)}
     *   <li>{@link ErrorResponseDto#hashCode()}
     * </ul>
     */
    @Test
    void testEquals2() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        assertEquals(errorResponseDto, errorResponseDto);
        int expectedHashCodeResult = errorResponseDto.hashCode();
        assertEquals(expectedHashCodeResult, errorResponseDto.hashCode());
    }

    /**
     * Methods under test:
     *
     * <ul>
     *   <li>{@link ErrorResponseDto#equals(Object)}
     *   <li>{@link ErrorResponseDto#hashCode()}
     * </ul>
     */
    @Test
    void testEquals3() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        ErrorResponseDto errorResponseDto2 = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());

        assertEquals(errorResponseDto, errorResponseDto2);
        int expectedHashCodeResult = errorResponseDto.hashCode();
        assertEquals(expectedHashCodeResult, errorResponseDto2.hashCode());
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals4() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("An error occurred", HttpStatus.CONTINUE,
                "An error occurred", LocalDate.of(1970, 1, 1).atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals5() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto(null, HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals6() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", null, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals7() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.SWITCHING_PROTOCOLS,
                "An error occurred", LocalDate.of(1970, 1, 1).atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals8() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "Api Path",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals9() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, null,
                LocalDate.of(1970, 1, 1).atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Method under test: {@link ErrorResponseDto#equals(Object)}
     */
    @Test
    void testEquals10() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.now().atStartOfDay());
        assertNotEquals(errorResponseDto, new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay()));
    }

    /**
     * Methods under test:
     *
     * <ul>
     *   <li>{@link ErrorResponseDto#equals(Object)}
     *   <li>{@link ErrorResponseDto#hashCode()}
     * </ul>
     */
    @Test
    void testEquals11() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto(null, HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        ErrorResponseDto errorResponseDto2 = new ErrorResponseDto(null, HttpStatus.CONTINUE, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());

        assertEquals(errorResponseDto, errorResponseDto2);
        int expectedHashCodeResult = errorResponseDto.hashCode();
        assertEquals(expectedHashCodeResult, errorResponseDto2.hashCode());
    }

    /**
     * Methods under test:
     *
     * <ul>
     *   <li>{@link ErrorResponseDto#equals(Object)}
     *   <li>{@link ErrorResponseDto#hashCode()}
     * </ul>
     */
    @Test
    void testEquals12() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", null, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());
        ErrorResponseDto errorResponseDto2 = new ErrorResponseDto("Api Path", null, "An error occurred",
                LocalDate.of(1970, 1, 1).atStartOfDay());

        assertEquals(errorResponseDto, errorResponseDto2);
        int expectedHashCodeResult = errorResponseDto.hashCode();
        assertEquals(expectedHashCodeResult, errorResponseDto2.hashCode());
    }

    /**
     * Methods under test:
     *
     * <ul>
     *   <li>{@link ErrorResponseDto#equals(Object)}
     *   <li>{@link ErrorResponseDto#hashCode()}
     * </ul>
     */
    @Test
    void testEquals13() {
        ErrorResponseDto errorResponseDto = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, null,
                LocalDate.of(1970, 1, 1).atStartOfDay());
        ErrorResponseDto errorResponseDto2 = new ErrorResponseDto("Api Path", HttpStatus.CONTINUE, null,
                LocalDate.of(1970, 1, 1).atStartOfDay());

        assertEquals(errorResponseDto, errorResponseDto2);
        int expectedHashCodeResult = errorResponseDto.hashCode();
        assertEquals(expectedHashCodeResult, errorResponseDto2.hashCode());
    }
}
