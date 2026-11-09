package de.z0rdak.yawp.gametest;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.api.events.flag.FlagEvents;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.world.entity.player.Player;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Records every flag check and result on the common {@link FlagEvents} bus.
 * Tests can use {@link #watch(Player)} to get a log of all checks and results caused by a specific player, to avoid test overlap.
 */
final class FlagRecorder {
    private static final FlagRecorder INSTANCE = new FlagRecorder();


    private final List<FlagCheckResult> results = new ArrayList<>();
    private final List<FlagCheckRequest> checks = new ArrayList<>();

    private FlagRecorder() {
        FlagEvents.ON_FLAG_CHECK.register(request -> {
            checks.add(request);
            return false; // observe only, the common bus can't cancel
        });
        FlagEvents.ON_FLAG_RESULT.register(result -> {
            results.add(result);
            return result;
        });
    }

    /**
     * Starts a log which only contains checks caused by the given player from now on.
     */
    static FlagLog watch(Player player) {
        return new FlagLog(INSTANCE, player.getUUID());
    }

    static final class FlagLog {
        private final FlagRecorder recorder;
        private final UUID playerId;
        private int checksStart;
        private int resultsStart;

        private FlagLog(FlagRecorder recorder, UUID playerId) {
            this.recorder = recorder;
            this.playerId = playerId;
            this.reset();
        }

        /** Forget everything recorded so far */
        void reset() {
            this.checksStart = recorder.checks.size();
            this.resultsStart = recorder.results.size();
        }

        private boolean isByPlayer(FlagCheckRequest request) {
            Player player = request.getPlayer();
            return player != null && player.getUUID().equals(this.playerId);
        }

        /** All flag checks of the player since the start of this log, in order */
        List<FlagCheckRequest> checks() {
            return recorder.checks.subList(checksStart, recorder.checks.size()).stream()
                .filter(this::isByPlayer).toList();
        }

        /** All evaluated results of the player since the start of this log, in order */
        List<FlagCheckResult> results() {
            return recorder.results.subList(resultsStart, recorder.results.size()).stream()
                .filter(result -> isByPlayer(result.getFlagCheck())).toList();
        }

        /** The checked flags in the order they were checked, may contain duplicates */
        List<RegionFlag> checkedFlags() {
            return checks().stream().map(FlagCheckRequest::getRegionFlag).toList();
        }

        boolean wasChecked(RegionFlag flag) {
            return checkedFlags().contains(flag);
        }

        /** The first result for this flag, if it was evaluated */
        Optional<FlagCheckResult> resultFor(RegionFlag flag) {
            return results().stream().filter(r -> r.getFlagCheck().getRegionFlag() == flag).findFirst();
        }

        boolean wasDenied(RegionFlag flag) {
            return resultFor(flag).map(r -> r.getFlagState() == FlagState.DENIED).orElse(false);
        }

        /** Readable summary for assertion messages, e.g. "[use-blocks=ALLOWED, place-blocks=DENIED]" */
        String describe() {
            return results().stream()
                .map(r -> r.getFlagCheck().getRegionFlag().name + "=" + r.getFlagState())
                .toList().toString();
        }
    }

}
