// Minimal JUCE + Gin stubs for compiling PluginProcessor.cpp without real JUCE.
// Only the types/methods actually used by PAPUEngine / PAPUAudioProcessor are stubbed.
#pragma once

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cstdint>
#include <cstdarg>
#include <cassert>
#include <algorithm>
#include <functional>
#include <initializer_list>
#include <map>
#include <memory>
#include <string>
#include <vector>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#define JUCE_IOS 0
#define JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(x)
#define JUCE_CALLTYPE
#define jassert(x) assert(x)

// ============================================================================
// juce namespace
// ============================================================================
namespace juce {

// --- String ---
class String {
    std::string s_;
public:
    String() = default;
    String(const char* c) : s_(c ? c : "") {}
    String(const std::string& str) : s_(str) {}
    String(int n) : s_(std::to_string(n)) {}

    const char* toRawUTF8() const { return s_.c_str(); }
    const std::string& toStdString() const { return s_; }

    bool operator==(const String& o) const { return s_ == o.s_; }
    bool operator!=(const String& o) const { return s_ != o.s_; }
    bool operator<(const String& o) const { return s_ < o.s_; }

    String operator+(const String& o) const { return String(s_ + o.s_); }
    friend String operator+(const char* lhs, const String& rhs) { return String(std::string(lhs) + rhs.s_); }

    static String formatted(const char* fmt, ...) {
        char buf[256];
        va_list args;
        va_start(args, fmt);
        vsnprintf(buf, sizeof(buf), fmt, args);
        va_end(args);
        return String(std::string(buf));
    }
};

// --- Array<T> ---
template<typename T>
class Array {
    std::vector<T> v_;
public:
    void add(T val) { v_.push_back(val); }
    T getLast() const { return v_.back(); }
    int size() const { return (int)v_.size(); }
    void clear() { v_.clear(); }
    void removeFirstMatchingValue(T val) {
        for (auto it = v_.begin(); it != v_.end(); ++it) {
            if (*it == val) { v_.erase(it); return; }
        }
    }
    T& operator[](int i) { return v_[i]; }
    const T& operator[](int i) const { return v_[i]; }
};

// --- NormalisableRange<T> ---
template<typename T>
struct NormalisableRange {
    T start, end, interval, skew;
    NormalisableRange() : start(0), end(1), interval(0), skew(1) {}
    NormalisableRange(T s, T e, T i, T sk) : start(s), end(e), interval(i), skew(sk) {}
};

// --- AudioBuffer<T> ---
template<typename T>
class AudioBuffer {
    std::vector<std::vector<T>> channels_;
    int numSamples_ = 0;
public:
    AudioBuffer() = default;
    AudioBuffer(int numChannels, int numSamples) : numSamples_(numSamples) {
        channels_.resize(numChannels, std::vector<T>(numSamples, T(0)));
    }
    int getNumSamples() const { return numSamples_; }
    int getNumChannels() const { return (int)channels_.size(); }
    T* getWritePointer(int ch, int offset = 0) { return channels_[ch].data() + offset; }
    const T* getReadPointer(int ch, int offset = 0) const { return channels_[ch].data() + offset; }
    void clear() {
        for (auto& ch : channels_)
            std::fill(ch.begin(), ch.end(), T(0));
    }
};

using AudioSampleBuffer = AudioBuffer<float>;

// --- MidiMessage ---
class MidiMessage {
public:
    enum Type { NoteOn, NoteOff, PitchWheel, AllNotesOff, Other };
private:
    int ch_ = 1;
    Type type_ = Other;
    int note_ = 0;
    int value_ = 0;
public:
    MidiMessage() = default;
    MidiMessage(int ch, Type t, int note, int val) : ch_(ch), type_(t), note_(note), value_(val) {}

    bool isNoteOn() const { return type_ == NoteOn; }
    bool isNoteOff() const { return type_ == NoteOff; }
    bool isPitchWheel() const { return type_ == PitchWheel; }
    bool isAllNotesOff() const { return type_ == AllNotesOff; }
    int getNoteNumber() const { return note_; }
    int getChannel() const { return ch_; }
    int getPitchWheelValue() const { return value_; }

    static MidiMessage noteOn(int ch, int note, float /*vel*/) {
        return {ch, NoteOn, note, 0};
    }
    static MidiMessage noteOff(int ch, int note) {
        return {ch, NoteOff, note, 0};
    }
    static MidiMessage pitchWheel(int ch, int val) {
        return {ch, PitchWheel, 0, val};
    }
    static MidiMessage allNotesOff(int ch) {
        return {ch, AllNotesOff, 0, 0};
    }
};

// --- MidiBuffer ---
struct MidiBufferEntry {
    MidiMessage msg;
    int samplePosition;
    MidiMessage getMessage() const { return msg; }
};

class MidiBuffer {
    std::vector<MidiBufferEntry> events_;
public:
    void addEvent(const MidiMessage& msg, int pos) {
        events_.push_back({msg, pos});
    }
    void clear() { events_.clear(); }
    auto begin() const { return events_.begin(); }
    auto end() const { return events_.end(); }
};

// --- OwnedArray<T> ---
template<typename T>
class OwnedArray {
    std::vector<std::unique_ptr<T>> v_;
public:
    void add(T* ptr) { v_.emplace_back(ptr); }
    T* operator[](int i) { return v_[i].get(); }
    const T* operator[](int i) const { return v_[i].get(); }
    int size() const { return (int)v_.size(); }

    // Range-based for: iterates over raw pointers
    struct Iterator {
        typename std::vector<std::unique_ptr<T>>::iterator it;
        T* operator*() { return it->get(); }
        Iterator& operator++() { ++it; return *this; }
        bool operator!=(const Iterator& other) const { return it != other.it; }
    };
    struct ConstIterator {
        typename std::vector<std::unique_ptr<T>>::const_iterator it;
        const T* operator*() { return it->get(); }
        ConstIterator& operator++() { ++it; return *this; }
        bool operator!=(const ConstIterator& other) const { return it != other.it; }
    };
    Iterator begin() { return {v_.begin()}; }
    Iterator end() { return {v_.end()}; }
    ConstIterator begin() const { return {v_.begin()}; }
    ConstIterator end() const { return {v_.end()}; }
};

// --- AudioProcessorEditor ---
class AudioProcessorEditor {
public:
    virtual ~AudioProcessorEditor() = default;
};

// --- AudioProcessor ---
class AudioProcessor {
public:
    virtual ~AudioProcessor() = default;
    virtual void prepareToPlay(double, int) {}
    virtual void releaseResources() {}
    virtual void processBlock(AudioSampleBuffer&, MidiBuffer&) {}
    virtual AudioProcessorEditor* createEditor() { return nullptr; }
    virtual bool hasEditor() const { return false; }
};

// --- MathConstants ---
template<typename T>
struct MathConstants {
    static constexpr T pi = T(M_PI);
};

// --- Random ---
class Random {
    uint64_t seed_;
public:
    Random() : seed_(1) {}
    explicit Random(int s) : seed_((uint64_t)s) {}

    // Simple xorshift64 matching no particular JUCE internals;
    // only used for gin::LFO random waveforms (which PAPU never uses — it's sine only).
    uint64_t next() { seed_ ^= seed_ << 13; seed_ ^= seed_ >> 7; seed_ ^= seed_ << 17; return seed_; }
    float nextFloat() { return (float)(next() % 100000) / 100000.0f; }
    int nextInt(int bound) { return (int)(next() % (uint64_t)bound); }

    static Random& getSystemRandom() { static Random r(42); return r; }
};

// --- Free functions ---
template<typename T>
T jmin(T a, T b) { return std::min(a, b); }

template<typename T>
T jmin(T a, T b, T c) { return std::min(a, std::min(b, c)); }

template<typename T>
T jlimit(T lo, T hi, T v) { return std::max(lo, std::min(hi, v)); }

inline int roundToInt(double v) { return (int)std::round(v); }

template<typename T, size_t N>
constexpr int numElementsInArray(T (&)[N]) { return (int)N; }

// --- MidiKeyboardState (unused, guarded by JUCE_IOS) ---
struct MidiKeyboardState {};

} // namespace juce

// ============================================================================
// gin namespace
// ============================================================================
namespace gin {

// --- getMidiNoteInHertz ---
inline double getMidiNoteInHertz(double noteNumber) {
    return 440.0 * std::pow(2.0, (noteNumber - 69.0) / 12.0);
}

// --- LFO (actual gin code from gin_lfo.h) ---
#include "gin_lfo.h"

// --- Parameter ---
struct Parameter {
    float value = 0;
    float min_ = 0, max_ = 1;
    float getUserRangeEnd() const { return max_; }
};

// --- ProcessorOptions ---
struct ProcessorOptions {
    ProcessorOptions& withAdditionalCredits(std::initializer_list<const char*>) { return *this; }
    ProcessorOptions& withMidiLearn() { return *this; }
};

// --- AudioFifo (stub: never has space, so writeMono is never called) ---
struct AudioFifo {
    AudioFifo(int, int) {}
    int getFreeSpace() const { return 0; }
    void writeMono(const float*, int) {}
};

// --- MidiLearn stub ---
struct MidiLearn {
    void processBlock(juce::MidiBuffer&, int) {}
};

// --- Processor (base class for PAPUAudioProcessor) ---
class Processor : public juce::AudioProcessor {
public:
    Processor(bool = false, ProcessorOptions = {}) {}
    ~Processor() override = default;

    using TextFunction = std::function<juce::String(const Parameter&, float)>;

    void addExtParam(const juce::String& uid, const char*, const char*, const char*,
                     juce::NormalisableRange<float> range, float defaultVal, float /*skew*/,
                     TextFunction /*textFn*/ = {}) {
        Parameter p;
        p.value = defaultVal;
        p.min_ = range.start;
        p.max_ = range.end;
        params_[uid] = p;
    }

    int parameterIntValue(const juce::String& uid) {
        return (int)params_[uid].value;
    }

    float parameterValue(const juce::String& uid) {
        return params_[uid].value;
    }

    void setParameterValue(const juce::String& uid, float v) {
        params_[uid].value = v;
    }

    void init() {}

    // Virtual overrides (unused in the harness but required by PluginProcessor.cpp)
    void prepareToPlay(double, int) override {}
    void releaseResources() override {}
    void processBlock(juce::AudioSampleBuffer&, juce::MidiBuffer&) override {}
    juce::AudioProcessorEditor* createEditor() override { return nullptr; }
    bool hasEditor() const override { return false; }
    const juce::String getName() const { return ""; }
    bool acceptsMidi() const { return true; }
    bool producesMidi() const { return false; }
    double getTailLengthSeconds() const { return 0; }
    int getNumPrograms() { return 1; }
    int getCurrentProgram() { return 0; }
    void setCurrentProgram(int) {}
    const juce::String getProgramName(int) { return ""; }
    void changeProgramName(int, const juce::String&) {}
    void getStateInformation(void*) {}
    void setStateInformation(const void*, int) {}

    MidiLearn dummyMidiLearn_;
    MidiLearn* midiLearn = &dummyMidiLearn_;

protected:
    std::map<juce::String, Parameter> params_;
};

} // namespace gin
